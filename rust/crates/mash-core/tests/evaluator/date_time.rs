//! Port of `evaluator/DateTimeTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(
        r#"'29th June 2017, 18:36:44'.toDateTime | select --hour --minute --second"#,
        r#"{ hour: 18, minute: 36, second: 44 }"#,
    ),
    Case::eq(r#"now.date"#, r#"today"#),
    Case::eq(
        r#"t = now; t0 = t.format.toDateTime; t0 == t0.format.toDateTime"#,
        r#"true"#,
    ),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[
    // Cannot find member 'toDateTime' in value of type String
    r#"'29th June 2017, 18:36:44'.toDateTime | select --hour --minute --second"#,
    // No binding for 'now'
    r#"now.date"#,
    // No binding for 'now'
    r#"t = now; t0 = t.format.toDateTime; t0 == t0.format.toDateTime"#,
];
