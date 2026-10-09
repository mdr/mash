//! Running tables of evaluator test cases (port of `AbstractEvaluatorTest`).

use std::panic::{AssertUnwindSafe, catch_unwind};
use std::rc::Rc;

use mash_core::eval::{Ctx, RunError, with_big_stack};
use mash_core::platform::TestPlatform;
use mash_core::value::Value;

#[derive(Debug)]
pub(crate) enum Case {
    /// Evaluates to the same value as `expected`
    Eq {
        input: &'static str,
        expected: &'static str,
        bare_words: bool,
    },
    /// Fails with an evaluation error
    Throws { input: &'static str, bare_words: bool },
    /// Evaluates without error
    Ok { input: &'static str, bare_words: bool },
    /// Cases run in an interpreter where `setup` has been run first
    Setup {
        setup: &'static str,
        cases: &'static [Case],
    },
}

impl Case {
    pub(crate) const fn eq(input: &'static str, expected: &'static str) -> Case {
        Case::Eq {
            input,
            expected,
            bare_words: false,
        }
    }

    #[allow(dead_code)]
    pub(crate) const fn eq_bare(input: &'static str, expected: &'static str) -> Case {
        Case::Eq {
            input,
            expected,
            bare_words: true,
        }
    }

    pub(crate) const fn throws(input: &'static str) -> Case {
        Case::Throws {
            input,
            bare_words: false,
        }
    }

    #[allow(dead_code)]
    pub(crate) const fn ok(input: &'static str) -> Case {
        Case::Ok {
            input,
            bare_words: false,
        }
    }

    pub(crate) const fn setup(setup: &'static str, cases: &'static [Case]) -> Case {
        Case::Setup { setup, cases }
    }

    fn input(&self) -> &'static str {
        match self {
            Case::Eq { input, .. } | Case::Throws { input, .. } | Case::Ok { input, .. } => input,
            Case::Setup { setup, .. } => setup,
        }
    }
}

fn interpreter() -> Ctx {
    Ctx::new(Rc::new(TestPlatform::default()))
}

/// Scala's `stripMargin`: remove leading whitespace and a `|` from each line
fn strip_margin(s: &str) -> String {
    s.lines()
        .map(|line| {
            let trimmed = line.trim_start();
            trimmed.strip_prefix('|').unwrap_or(line)
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn run_source(ctx: &mut Ctx, source: &str, bare_words: bool) -> Result<Value, RunError> {
    ctx.settings.bare_words = bare_words;
    let result = ctx.run("test", &strip_margin(source));
    ctx.settings.bare_words = false;
    result
}

/// Ok(()) if the case passes, else what went wrong
fn check(ctx: &mut Ctx, case: &Case) -> Result<(), String> {
    match case {
        Case::Eq {
            input,
            expected,
            bare_words,
        } => {
            let actual = run_source(ctx, input, *bare_words).map_err(|e| format!("error: {e}"))?;
            let expected =
                run_source(ctx, expected, *bare_words).map_err(|e| format!("error evaluating expected value: {e}"))?;
            if actual == expected {
                Ok(())
            } else {
                Err(format!("got {actual:?}, expected {expected:?}"))
            }
        }
        Case::Throws { input, bare_words } => match run_source(ctx, input, *bare_words) {
            Err(RunError::Eval(_)) => Ok(()),
            Err(RunError::Syntax(e)) => Err(format!("syntax error rather than evaluation error: {e:?}")),
            Ok(value) => Err(format!("expected an error, got {value:?}")),
        },
        Case::Ok { input, bare_words } => run_source(ctx, input, *bare_words)
            .map(|_| ())
            .map_err(|e| format!("error: {e}")),
        Case::Setup { .. } => unreachable!(),
    }
}

/// Run all the cases. Cases listed in `deferred` (by input) must fail; any other failure fails the test.
pub(crate) fn run(cases: &'static [Case], deferred: &'static [&'static str]) {
    let report = with_big_stack(move || {
        let mut failures = vec![];
        let mut passing_deferred = vec![];
        let mut record = |case: &Case, result: Result<(), String>| {
            let is_deferred = deferred.contains(&case.input());
            match (result, is_deferred) {
                (Err(problem), false) => failures.push(format!("{:?}: {problem}", case.input())),
                (Ok(()), true) => passing_deferred.push(format!("{:?}", case.input())),
                _ => {}
            }
        };
        let guarded = |ctx: &mut Ctx, case: &Case| {
            catch_unwind(AssertUnwindSafe(|| check(ctx, case))).unwrap_or_else(|_| Err("panicked".into()))
        };
        for case in cases {
            match case {
                Case::Setup { setup, cases } => {
                    let mut ctx = interpreter();
                    if let Err(e) = run_source(&mut ctx, setup, false) {
                        record(case, Err(format!("setup failed: {e}")));
                        continue;
                    }
                    for case in *cases {
                        let result = guarded(&mut ctx, case);
                        record(case, result);
                    }
                }
                _ => {
                    let result = guarded(&mut interpreter(), case);
                    record(case, result);
                }
            }
        }
        (failures, passing_deferred)
    });
    let (failures, passing_deferred) = report;
    let mut message = String::new();
    if !failures.is_empty() {
        message += &format!("{} failing case(s):\n  {}\n", failures.len(), failures.join("\n  "));
    }
    if !passing_deferred.is_empty() {
        message += &format!(
            "{} deferred case(s) now pass; remove them from DEFERRED:\n  {}\n",
            passing_deferred.len(),
            passing_deferred.join("\n  ")
        );
    }
    assert!(message.is_empty(), "{message}");
}
