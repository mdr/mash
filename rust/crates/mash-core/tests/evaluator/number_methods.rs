//! Port of `evaluator/NumberMethodsTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"1.to 5"#, r#"[1, 2, 3, 4, 5]"#),
    Case::eq(r#"1.to 10 --step=2"#, r#"[1, 3, 5, 7, 9]"#),
    Case::eq(r#"5.to 1 --step=-1"#, r#"[5, 4, 3, 2, 1]"#),
    Case::eq(r#"1.until 5"#, r#"[1, 2, 3, 4]"#),
    Case::eq(r#"1.until 10 --step=2"#, r#"[1, 3, 5, 7, 9]"#),
    Case::eq(r#"5.until 1 --step=-1"#, r#"[5, 4, 3, 2]"#),
    Case::eq(r#"a = 0; 5.times (a += 1); a"#, r#"5"#),
    Case::eq(r#"a = 0; (a += 1) | 5.times"#, r#"[1, 2, 3, 4, 5]"#),
    Case::eq(r#"2.power 8"#, r#"256"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[
    // Cannot find member 'to' in value of type Number
    r#"1.to 5"#,
    // Cannot find member 'to' in value of type Number
    r#"1.to 10 --step=2"#,
    // Cannot find member 'to' in value of type Number
    r#"5.to 1 --step=-1"#,
    // Cannot find member 'until' in value of type Number
    r#"1.until 5"#,
    // Cannot find member 'until' in value of type Number
    r#"1.until 10 --step=2"#,
    // Cannot find member 'until' in value of type Number
    r#"5.until 1 --step=-1"#,
    // Cannot find member 'times' in value of type Number
    r#"a = 0; 5.times (a += 1); a"#,
    // Cannot find member 'times' in value of type Number
    r#"a = 0; (a += 1) | 5.times"#,
    // Cannot find member 'power' in value of type Number
    r#"2.power 8"#,
];
