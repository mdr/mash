//! Port of `evaluator/LastParamTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"def foo n... m = m; foo 1 2 3"#, r#"3"#),
    Case::eq(r#"def foo n... m = m; foo 3"#, r#"3"#),
    Case::eq(r#"def foo (n = 10) m = m; foo 3"#, r#"3"#),
    Case::eq(
        r#"def fun first (second = 10) = { first, second }; fun 1 2"#,
        r#"{ first: 1, second: 2 }"#,
    ),
    Case::eq(
        r#"def fun first (second = 10) = { first, second }; fun --first=1"#,
        r#"{ first: 1, second: 10 }"#,
    ),
    Case::eq(
        r#"def fun first (second = 10) = { first, second }; fun 1"#,
        r#"{ first: 1, second: 10 }"#,
    ),
    Case::eq(
        r#"def fun args... (arg = 10) = { args, arg }; fun"#,
        r#"{ args: [], arg: 10 }"#,
    ),
    Case::eq(
        r#"def fun args... (arg = 10) = { args, arg }; fun 1"#,
        r#"{ args: [], arg: 1 }"#,
    ),
    Case::eq(
        r#"def fun args... (arg = 10) = { args, arg }; fun 1 2"#,
        r#"{ args: [1], arg: 2 }"#,
    ),
    Case::eq(
        r#"def fun args... (arg = 10) = { args, arg }; fun --arg=20"#,
        r#"{ args: [], arg: 20 }"#,
    ),
    Case::eq(
        r#"def fun args... (arg = 10) = { args, arg }; fun 1 --arg=20"#,
        r#"{ args: [1], arg: 20 }"#,
    ),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
