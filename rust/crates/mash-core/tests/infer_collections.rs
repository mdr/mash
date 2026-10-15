//! Type inference and argument completion for the collections builtins: cases beyond the Scala
//! TypeInferencerTest, especially completion specs (Scala `getCompletionSpecs`).

use std::rc::Rc;

use mash_core::ast::{Arg, ExprKind, SourceUnit};
use mash_core::complete::CompletionSpec;
use mash_core::eval::{Ctx, with_big_stack};
use mash_core::infer::{self, BuiltinFunctionType, Inferred, Type, TypedArg, TypedArgs, ValueInfo};
use mash_core::platform::TestPlatform;

fn ctx() -> Ctx {
    Ctx::new(Rc::new(TestPlatform::default()))
}

fn inferred_type(ctx: &mut Ctx, input: &str) -> String {
    let compiled = ctx.compile_forgiving(SourceUnit::new("test", input));
    let inferred = infer::infer(ctx, &compiled.program.body);
    match inferred.types.get(&compiled.program.body.id) {
        Some(ty) => format!("{ty:?}"),
        None => "<no type>".into(),
    }
}

/// Typed arguments of a call, from what was inferred
fn typed_args(inferred: &Inferred, args: &[Arg]) -> TypedArgs {
    let info = |id| ValueInfo {
        value: inferred.constants.get(&id).cloned(),
        ty: inferred.types.get(&id).cloned(),
    };
    TypedArgs {
        args: args
            .iter()
            .map(|arg| match arg {
                Arg::Positional(e) => TypedArg::Positional(info(e.id)),
                Arg::ShortFlags { flags, .. } => TypedArg::ShortFlags(flags.clone()),
                Arg::LongFlag { name, value, .. } => {
                    TypedArg::LongFlag(name.clone(), value.as_ref().map(|e| info(e.id)))
                }
            })
            .collect(),
    }
}

/// The completion specs for argument `arg_pos` of the call that is the whole of `input`
fn completion_specs(input: &str, arg_pos: usize) -> String {
    let mut ctx = ctx();
    let compiled = ctx.compile_forgiving(SourceUnit::new("test", input));
    let inferred = infer::infer(&ctx, &compiled.program.body);
    let ExprKind::Invocation { function, args, .. } = &compiled.program.body.kind else {
        panic!("not a call: {input}")
    };
    let typed = typed_args(&inferred, args);
    let specs: Vec<CompletionSpec> = match inferred.types.get(&function.id) {
        Some(Type::BuiltinFunction(BuiltinFunctionType::Builtin(f))) => f.completion_specs(arg_pos, &typed),
        Some(Type::BoundBuiltinMethod(target, m)) => m.completion_specs(arg_pos, Some(target), &typed),
        other => panic!("{input}: function has type {other:?}"),
    };
    format!("{specs:?}")
}

#[test]
fn inference() {
    let cases = [
        // map over strings: a string if the function returns strings, else a list
        ("map (_ + 'x') 'abc'", "core.String"),
        ("map (_ => 1) 'abc'", "[core.Number]"),
        ("map (_.foo) [{ foo: 1 }]", "[core.Number]"),
        ("[{ foo: 1 }].map (_.foo)", "[core.Number]"),
        ("[[1], [2]] | flatten", "[core.Number]"),
        ("[1, 2].flatMap (n => [{ a: n }])", "[{a: core.Number}]"),
        ("[1, 2] | each (n => n)", "core.Unit"),
        ("[1, 2] | all (_ > 1)", "core.Boolean"),
        ("[1, 2] | any (_ > 1)", "core.Boolean"),
        ("[1, 2] | countMatches (_ > 1)", "core.Number"),
        ("[1, 2] | contains 1", "core.Boolean"),
        ("[1, 2] | nonEmpty", "core.Boolean"),
        ("[1, 2] | length", "core.Number"),
        ("[1, 2] | count", "core.Number"),
        ("[1, 2] | join", "core.String"),
        ("[1, 2] | unique", "[core.Number]"),
        ("'aab' | unique", "core.String"),
        ("[3, 1] | sort", "[core.Number]"),
        ("[3, 1] | sortBy (_)", "[core.Number]"),
        ("[3, 1] | sortBy (_) | first", "core.Number"),
        ("[1, 2] | skipWhile (_ < 2)", "[core.Number]"),
        ("[1, 2] | takeWhile (_ < 2)", "[core.Number]"),
        ("[1, 2] | skipUntil (_ < 2)", "[core.Number]"),
        ("'abc' | whereNot (_ == 'a')", "core.String"),
        ("42 | where (_ > 1)", "[core.Any]"),
        ("[1, 2] | maxBy (_)", "core.Number"),
        ("[1, 2] | minBy (_)", "core.Number"),
        ("'abc' | first 2", "core.String"),
        ("[1, 2, 3] | last 2", "[core.Number]"),
        ("[1, 2] | reduce (x y => x + y)", "core.Number"),
        ("[1, 2] | reduce (x y => x + [y]) []", "[core.Number]"),
        ("['a'] | sum", "core.String"),
        ("[1] | sum", "core.Number"),
        ("[{ a: 1 }] | sum", "{a: core.Number}"),
        ("zip [1] ['a']", "[[core.Number]]"),
        ("zip [] ['a']", "[[core.Any]]"),
        ("[1, 2] | chunked 2", "[[core.Number]]"),
        ("[{ a: 1 }] | groupBy (_.a)", "core.Object"),
        (
            "[{ a: 1 }] | groupBy (_.a) --groups",
            "[collections.Group[core.Number, {a: core.Number}]]",
        ),
        (
            "[{ a: 1 }] | groupBy (_.a) --groups | first | .values",
            "[{a: core.Number}]",
        ),
        ("[{ a: 1 }] | groupBy (_.a) --groups | first | .count", "core.Number"),
        ("[{ a: 1 }] | countBy (_.a)", "core.Object"),
        (
            "[{ a: 1 }] | countBy (_.a) --groups",
            "[{key: core.Number, count: core.Number}]",
        ),
        ("[{ a: 1 }] | countBy (_.a) --groups | first | .key", "core.Number"),
        ("'abc'.countBy (c => c) --groups | first | .count", "core.Number"),
        ("[1, 2] | counts", "core.Object"),
        ("['a=1'] | parse '{k}={v}'", "[{k: core.String, v: core.String}]"),
        ("'a=1' | parse -r '(?<k>\\w)='", "[{k: core.String}]"),
        ("'a1'.scan '\\d'", "[core.String]"),
        (
            "[{ a: 1, b: 'x' }] | rename { a: 'n' }",
            "[{n: core.Number, b: core.String}]",
        ),
        ("{ a: 1 }.rename { a: 'n' }", "{n: core.Number}"),
        (
            "[{ a: 'x', b: 'y' }] | update 'a' (.length)",
            "[{a: core.Any, b: core.String}]",
        ),
        ("[{ a: 'x' }] | default 'b' 0", "[{a: core.String, b: core.Any}]"),
        ("'a=1'.scan '(\\w)=(\\d)'", "[[core.String]]"),
        ("[1, 2].counts --groups", "[{key: core.Number, count: core.Number}]"),
        (
            "'abc'.counts --groups | first | .key",
            "core.String tagged core.Character",
        ),
        ("[1, 2] | grep 1", "[core.Number]"),
        ("'a`nb' | grep 'a'", "[core.String]"),
        ("{ a: 1 } | grep 'a'", "core.Object"),
        ("[[1]] | transpose", "[[core.Number]]"),
        ("[{ a: 1 }] | transpose", "{a: [core.Number]}"),
        // As in Scala: a list of class instances transposes to an object of the class's fields (none for Number)
        ("[1] | transpose", "{}"),
        ("{ a: 1, b: 'x' } | select 'a'", "{a: core.Number}"),
        ("{ a: 1, b: 'x' } | select --c=(_.b)", "{c: core.String}"),
        ("{ a: 1, b: 'x' } | select --a", "{a: core.Number}"),
        ("{ a: 1 } | select --add --c=(_.a)", "{a: core.Number, c: core.Number}"),
        ("[{ a: 1, b: 'x' }].select 'b'", "[{b: core.String}]"),
        ("[1].append 2", "[core.Number]"),
        ("[1].prepend 2", "[core.Number]"),
        ("[1].intersect [2]", "[core.Number]"),
        ("List 'a' 'b'", "[core.String]"),
        ("max 'a' 'b'", "core.String"),
        ("[1, 2].max", "core.Number"),
        ("{ a: 1, b: 2 } | first 2", "{a: core.Number, b: core.Number}"),
    ];
    let failures = with_big_stack(move || {
        let mut failures = vec![];
        for (input, expected) in cases {
            let actual = inferred_type(&mut ctx(), input);
            if actual != expected {
                failures.push(format!("{input:?}: got {actual}, expected {expected}"));
            }
        }
        failures
    });
    assert!(
        failures.is_empty(),
        "{} failing:\n  {}",
        failures.len(),
        failures.join("\n  ")
    );
}

#[test]
fn completion_specs_for_function_arguments() {
    let failures = with_big_stack(|| {
        let cases = [
            // The function argument of map and friends: members of the elements
            ("map (_.foo) [{ foo: 1 }]", 0, "[Members({foo: core.Number})]"),
            ("map (_.foo) [{ foo: 1 }]", 1, "[]"),
            ("where (_.foo) [{ foo: 1 }]", 0, "[Members({foo: core.Number})]"),
            ("sortBy 'foo' [{ foo: 1 }]", 0, "[Members({foo: core.Number})]"),
            ("[{ foo: 1 }] | groupBy 'foo'", 0, "[Members({foo: core.Number})]"),
            ("[{ foo: 1 }] | sumBy 'foo'", 0, "[Members({foo: core.Number})]"),
            ("flatMap 'foo' [{ foo: [1] }]", 0, "[Members({foo: [core.Number]})]"),
            ("[{ foo: 1 }].map 'foo'", 0, "[Members({foo: core.Number})]"),
            // Not for non-lists (as Scala)
            ("map 'foo' 42", 0, "[]"),
            // select: all arguments but the input
            (
                "select 'foo' [{ foo: 1, bar: 2 }]",
                0,
                "[Members({foo: core.Number, bar: core.Number})]",
            ),
            ("select 'foo' { foo: 1 }", 0, "[Members({foo: core.Number})]"),
            ("select 'foo' { foo: 1 }", 1, "[]"),
            ("[{ foo: 1 }].select 'foo'", 0, "[Members({foo: core.Number})]"),
            // deselect: the field names of the target
            ("deselect 'foo' { foo: 1, bar: 2 }", 0, r#"[Items(["foo", "bar"])]"#),
            ("deselect 'foo' [{ foo: 1 }]", 0, r#"[Items(["foo"])]"#),
            ("deselect 'foo' [{ foo: 1 }]", 1, "[]"),
        ];
        let mut failures = vec![];
        for (input, pos, expected) in cases {
            let actual = completion_specs(input, pos);
            if actual != expected {
                failures.push(format!("{input:?} at {pos}: got {actual}, expected {expected}"));
            }
        }
        failures
    });
    assert!(
        failures.is_empty(),
        "{} failing:\n  {}",
        failures.len(),
        failures.join("\n  ")
    );
}
