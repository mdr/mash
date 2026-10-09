//! Port of `evaluator/NaturalJoinTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"[].naturalJoin []"#, r#"[]"#),
    Case::eq(r#"[{ bar: 100 }].naturalJoin [{ foo: 42 }]"#, r#"[]"#),
    Case::eq(
        r#"[{ foo: 12, bar: 100 }, { foo: 12, bar: 200 }].naturalJoin [{ foo: 12, baz: 100 }, { foo: 12, baz: 500 }]"#,
        r#"[{ foo: 12, bar: 100, baz: 100 }, { foo: 12, bar: 100, baz: 500 }, { foo: 12, bar: 200, baz: 100 }, { foo: 12, bar: 200, baz: 500 }]"#,
    ),
    Case::eq(
        r#"[{ a: 1, b: 2 }, { y: 3 }].naturalJoin [{ a: 1, c: 10 }, { z: 4 }]"#,
        r#"[{ a: 1, b: 2, c: 10 }]"#,
    ),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
