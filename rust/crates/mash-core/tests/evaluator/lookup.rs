//! Port of `evaluator/LookupTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"[1, 2, 3][0]"#, r#"1"#),
    Case::throws(r#"[1, 2, 3][3]"#),
    Case::eq(r#"[1, 2, 3][-1]"#, r#"3"#),
    Case::eq(r#"[1, 2, 3] | _[0]"#, r#"1"#),
    Case::eq(r#"'bar'[0]"#, r#"'b'"#),
    Case::throws(r#"'bar'[3]"#),
    Case::eq(r#"{ foo: 42 }['foo'] "#, r#"42"#),
    Case::throws(r#"{ foo: 42 }['bar'] "#),
    Case::eq(r#"{ (42): 'foo' }[42]"#, r#"'foo'"#),
    Case::eq(r#"{ (true): 'foo' }[true]"#, r#"'foo'"#),
    Case::eq(r#"{ ({ bar: 100 }): 'foo' }[{ bar: 100 }]"#, r#"'foo'"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
