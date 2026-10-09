//! Port of `evaluator/ArithmeticTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"'xy' * 3"#, r#"'xyxyxy'"#),
    Case::eq(r#"3 * 'xy'"#, r#"'xyxyxy'"#),
    Case::eq(r#"'x' * 1"#, r#"'x'"#),
    Case::eq(r#"'x' * 0"#, r#"''"#),
    Case::eq(r#"[1, 2] * 3"#, r#"[1, 2, 1, 2, 1, 2]"#),
    Case::eq(r#"3 * [1, 2]"#, r#"[1, 2, 1, 2, 1, 2]"#),
    Case::eq(r#"[1] * 1"#, r#"[1]"#),
    Case::eq(r#"[1] * 0"#, r#"[]"#),
    Case::eq(r#"3 * 1.second | .tag"#, r#"time.Seconds"#),
    Case::eq(r#"1.second * 3 | .tag"#, r#"time.Seconds"#),
    Case::eq(r#"1 * 2 + 3 * 4"#, r#"14"#),
    Case::eq(r#"3 * 100 / 3"#, r#"100"#),
    Case::eq(r#"10 / 10 / 10"#, r#"0.1"#),
    Case::eq(r#"1 - 2 - 3"#, r#"-4"#),
    Case::eq(r#"'foo' + 42"#, r#"'foo42'"#),
    Case::eq(r#"42 + 'foo'"#, r#"'42foo'"#),
    Case::eq(r#"[1, 2, 3] + [4, 5]"#, r#"[1, 2, 3, 4, 5]"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[
    // Cannot find member 'second' in value of type Number
    r#"3 * 1.second | .tag"#,
    // Cannot find member 'second' in value of type Number
    r#"1.second * 3 | .tag"#,
];
