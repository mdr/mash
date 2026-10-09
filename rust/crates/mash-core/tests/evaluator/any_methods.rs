//! Port of `evaluator/AnyMethodsTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"42.in [1, 2, 3]"#, r#"false"#),
    Case::eq(r#"2.in [1, 2, 3]"#, r#"true"#),
    Case::eq(r#"null.in [1, 2, 3]"#, r#"false"#),
    Case::eq(r#"null.in [null]"#, r#"true"#),
    Case::eq(r#"null.toString"#, r#" 'null' "#),
    Case::eq(r#"2.toString"#, r#" '2' "#),
    Case::eq(r#"().toString"#, r#"''"#),
    Case::eq(r#"true.toString"#, r#" 'true' "#),
    Case::eq(r#"[1, 2, 3].toString"#, r#"'[1, 2, 3]'"#),
    Case::eq(r#" "foo".toString.tag "#, r#" "foo".tag "#),
    Case::eq(r#"{ foo: 42 }"#, r#"{ foo: 42 }"#),
    Case::eq(r#"42.isA Number"#, r#"true"#),
    Case::eq(r#"42.isA Any"#, r#"true"#),
    Case::eq(r#"42.isA String"#, r#"false"#),
    Case::eq(r#"42.isA Object"#, r#"false"#),
    Case::eq(r#"null.isA Object"#, r#"false"#),
    Case::eq(r#"null.isA Null"#, r#"true"#),
    Case::eq(r#"null.isA Any"#, r#"true"#),
    Case::eq(r#"class A; A.new.isA A"#, r#"true"#),
    Case::eq(r#"class A; A.new.isA Object"#, r#"true"#),
    Case::eq(r#"class A; A.new.isA Any"#, r#"true"#),
    Case::eq(r#"class A; {}.isA A"#, r#"false"#),
    Case::eq(r#"class A; A.isA A"#, r#"false"#),
    Case::eq(r#"class A; A.isA Class"#, r#"true"#),
    Case::eq(r#"3.days.isA 3.days.tag"#, r#"true"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[
    // Cannot find member 'tag' in value of type String
    r#" "foo".toString.tag "#,
    // Cannot find member 'days' in value of type Number
    r#"3.days.isA 3.days.tag"#,
];
