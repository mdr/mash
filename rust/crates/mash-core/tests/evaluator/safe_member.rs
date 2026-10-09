//! Port of `evaluator/SafeMemberTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"null?.noSuchMember"#, r#"null"#),
    Case::eq(r#"42?.noSuchMember"#, r#"null"#),
    Case::eq(r#"{}?.noSuchMember"#, r#"null"#),
    Case::eq(r#"null?.toString"#, r#"'null'"#),
    Case::eq(r#"[null, { foo: 42 }] | map (_?.foo)"#, r#"[null, 42]"#),
    Case::eq(r#"[{ foo: { bar: 42 } }, { foo: null }].foo?.bar"#, r#"[42, null]"#),
    Case::eq(r#"[{ foo: 42 }, { bar: 12 }]?.foo"#, r#"[42, null]"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
