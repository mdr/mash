//! Port of `evaluator/EqualityTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"1 == 1"#, r#"true"#),
    Case::eq(r#"1 == 2"#, r#"false"#),
    Case::eq(r#"1 != 1"#, r#"false"#),
    Case::eq(r#"1 != 2"#, r#"true"#),
    Case::eq(r#"{ foo: 42, bar: 100 } == { bar: 100, foo: 42 }"#, r#"true"#),
    Case::eq(r#"{ (42): 'foo', bar: 100 } == { bar: 100, (42): 'foo' }"#, r#"true"#),
    Case::eq(r#"class Box n; Box 42 == { n: 42 }"#, r#"false"#),
    Case::eq(
        r#"class Box n; b1 = Box 42; class Box n; b2 = Box 42; b1 == b2"#,
        r#"false"#,
    ),
    Case::eq(r#"(class Box n) == (class Box n)"#, r#"false"#),
    Case::eq(r#"class Box n; Box == Box"#, r#"true"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
