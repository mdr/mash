//! Port of `evaluator/CharacterTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"'1234'.all (.isDigit)"#, r#"true"#),
    Case::eq(r#"'12b34'.all (.isDigit)"#, r#"false"#),
    Case::eq(r#"'abc'[0].tag"#, r#"Character"#),
    Case::eq(r#"'1'.toCharacter.isDigit"#, r#"true"#),
    Case::throws(r#"'123'.toCharacter"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[
    // Cannot find member 'all' in value of type String
    r#"'1234'.all (.isDigit)"#,
    // Cannot find member 'all' in value of type String
    r#"'12b34'.all (.isDigit)"#,
    // Cannot find member 'tag' in value of type String
    r#"'abc'[0].tag"#,
    // Cannot find member 'toCharacter' in value of type String
    r#"'1'.toCharacter.isDigit"#,
];
