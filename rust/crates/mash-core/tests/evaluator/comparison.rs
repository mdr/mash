//! Port of `evaluator/ComparisonTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"1 < 2"#, r#"true"#),
    Case::eq(r#"1 < 1"#, r#"false"#),
    Case::eq(r#"1 < 0"#, r#"false"#),
    Case::eq(r#"1 <= 2"#, r#"true"#),
    Case::eq(r#"1 <= 1"#, r#"true"#),
    Case::eq(r#"1 <= 0"#, r#"false"#),
    Case::eq(r#"1 > 2"#, r#"false"#),
    Case::eq(r#"1 > 1"#, r#"false"#),
    Case::eq(r#"1 > 0"#, r#"true"#),
    Case::eq(r#"1 >= 2"#, r#"false"#),
    Case::eq(r#"1 >= 1"#, r#"true"#),
    Case::eq(r#"1 >= 0"#, r#"true"#),
    Case::eq(r#"[] < [1]"#, r#"true"#),
    Case::eq(r#"[1] < [1, 2]"#, r#"true"#),
    Case::eq(r#"1 < 2 < 3"#, r#"true"#),
    Case::eq(r#"1 <= 2 < 3"#, r#"true"#),
    Case::eq(r#"1 < 100 < 3"#, r#"false"#),
    Case::eq(r#"1 < 2 < -100"#, r#"false"#),
    Case::eq(r#"1 < -10 < 100"#, r#"false"#),
    Case::eq(r#"a = 0; -100 < (a = a + 1; a) < 100; a"#, r#"1"#),
    Case::eq(r#"a = 0; (a += 1; 100) < (a += 1; -100) < (a += 1; -1000); a"#, r#"3"#),
    Case::eq(r#"now > 3.days.ago"#, r#"true"#),
    Case::eq(r#"now < 3.days.ago"#, r#"false"#),
    Case::eq(r#"now.date > 3.days.ago.date"#, r#"true"#),
    Case::eq(r#"now.date < 3.days.ago.date"#, r#"false"#),
    Case::eq(r#"null < 1"#, r#"false"#),
    Case::eq(r#"null <= 1"#, r#"false"#),
    Case::eq(r#"null > 1"#, r#"false"#),
    Case::eq(r#"null >= 1"#, r#"false"#),
    Case::eq(r#"1 < null"#, r#"false"#),
    Case::eq(r#"1 <= null"#, r#"false"#),
    Case::eq(r#"1 > null"#, r#"false"#),
    Case::eq(r#"1 >= null"#, r#"false"#),
    Case::eq(r#"null < null"#, r#"false"#),
    Case::eq(r#"null < null < null"#, r#"false"#),
    Case::eq(
        r#"() < true < 1 < 'foo' < now < reverse < [].sortBy < [] < {} < String"#,
        r#"true"#,
    ),
    Case::eq(r#"{ foo: 1 } < { foo: 2 }"#, r#"true"#),
    Case::eq(r#"{ foo: 1 } < { foo: 1, bar: 1 }"#, r#"true"#),
    Case::eq(r#"{ foo: 1, bar: 1 } < { foo: 1, bar: 2 }"#, r#"true"#),
    Case::eq(r#"{ foo: 1, bar: 1 } < { foo: 1, car: 1 }"#, r#"true"#),
    Case::eq(r#"{ a: 1, b: 1 } < { b: 1, a: 1 }"#, r#"true"#),
    Case::eq(r#"false < true"#, r#"true"#),
    Case::eq(r#"false < false"#, r#"false"#),
    Case::eq(r#"true < false"#, r#"false"#),
    Case::eq(r#"true < true"#, r#"false"#),
    Case::eq(r#"false <= false"#, r#"true"#),
    Case::eq(r#"true <= true"#, r#"true"#),
    Case::eq(r#"now > 1.day.ago"#, r#"true"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[
    // No binding for 'now'
    r#"now > 3.days.ago"#,
    // No binding for 'now'
    r#"now < 3.days.ago"#,
    // No binding for 'now'
    r#"now.date > 3.days.ago.date"#,
    // No binding for 'now'
    r#"now.date < 3.days.ago.date"#,
    // No binding for 'now'
    r#"() < true < 1 < 'foo' < now < reverse < [].sortBy < [] < {} < String"#,
    // No binding for 'now'
    r#"now > 1.day.ago"#,
];
