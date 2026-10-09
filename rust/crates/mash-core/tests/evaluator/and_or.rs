//! Port of `evaluator/AndOrTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"true and true"#, r#"true"#),
    Case::eq(r#"true and false"#, r#"false"#),
    Case::eq(r#"false and true"#, r#"false"#),
    Case::eq(r#"false and false"#, r#"false"#),
    Case::eq(r#"true or true"#, r#"true"#),
    Case::eq(r#"true or false"#, r#"true"#),
    Case::eq(r#"false or true"#, r#"true"#),
    Case::eq(r#"false or false"#, r#"false"#),
    Case::eq(r#"true or null.bang"#, r#"true"#),
    Case::eq(r#"false and null.bang"#, r#"false"#),
    Case::eq(r#"{} or 42"#, r#"42"#),
    Case::eq(r#"{ a: 1 } or 42"#, r#"{ a: 1 }"#),
    Case::eq(r#"[] or 42"#, r#"42"#),
    Case::eq(r#"[1] or 42"#, r#"[1]"#),
    Case::eq(r#"0 or 42"#, r#"42"#),
    Case::eq(r#"1 or 42"#, r#"1"#),
    Case::eq(r#"null or 42"#, r#"42"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
