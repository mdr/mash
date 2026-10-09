//! Port of `evaluator/FlagParamTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"def foo (@flag m = 10) n = m + n; foo 3"#, r#"13"#),
    Case::eq(r#"def foo (@flag m) n = m + n; foo 3 --m=20"#, r#"23"#),
    Case::eq(
        r#"def twice (@lazy @flag body) = (body; body); a = 0; twice --body=(a += 1); a"#,
        r#"2"#,
    ),
    Case::throws(r#"def twice (@lazy @flag body) = (body; body); a = 0; twice (a += 1); a"#),
    Case::eq(
        r#"def doSomething (@flag @(shortFlag 'd') dryRun = false) =
  if dryRun then 'Dry run' else 'For reals'
doSomething -d
 "#,
        r#"'Dry run'"#,
    ),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
