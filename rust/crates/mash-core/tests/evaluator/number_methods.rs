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
    // New: rounding, halves away from zero
    Case::eq(r#"[2.5.round, (-2.5).round, 2.4.round, 3.round]"#, r#"[3, -3, 2, 3]"#),
    Case::eq(
        r#"[3.14159.round 2, 1234.round -2, 2.675.round 0]"#,
        r#"[3.14, 1200, 3]"#,
    ),
    Case::throws(r#"3.14.round 1.5"#),
    Case::eq(
        r#"[2.7.floor, (-2.5).floor, 2.1.ceil, (-2.5).ceil]"#,
        r#"[2, -3, 3, -2]"#,
    ),
    Case::eq(r#"[(-3).abs, 3.abs, (-2.5).abs]"#, r#"[3, 3, 2.5]"#),
    // The remainder has the divisor's sign
    Case::eq(
        r#"[7.mod 3, (-7).mod 3, 7.mod -3, 7.5.mod 2, 6.mod 3]"#,
        r#"[1, 2, -2, 1.5, 0]"#,
    ),
    Case::throws(r#"7.mod 0"#),
    // Tags are kept
    Case::eq(r#"2.6.seconds.round"#, r#"3.seconds"#),
    Case::eq(r#"(-1500).bytes.abs.tag"#, r#"core.Bytes"#),
    Case::eq(r#"90.minutes.mod 60"#, r#"30.minutes"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
