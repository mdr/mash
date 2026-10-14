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
    // DIFFERENT: a string in a container is quoted where it would be ambiguous (Scala showed `[, London, UK]`)
    Case::eq(
        r#"['a', 'b c', '', 'London, UK', ' x', 'k: v', 'n`nl'].toString"#,
        r#"'[a, b c, "", "London, UK", " x", "k: v", "n\nl"]'"#,
    ),
    Case::eq(r#"{ a: '', 'b,c': 1 }.toString"#, r#"'{ a: "", "b,c": 1 }'"#),
    // A colon is ambiguous only followed by a space, or at the end
    Case::eq(
        r#"['::1', 'http://a.com/x', '12:30', 'k:'].toString"#,
        r#"'[::1, http://a.com/x, 12:30, "k:"]'"#,
    ),
    Case::eq(r#"'plain, as it is'.toString"#, r#"'plain, as it is'"#),
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
const DEFERRED: &[&str] = &[];
