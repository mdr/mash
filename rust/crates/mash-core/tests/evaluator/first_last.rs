//! Port of `evaluator/FirstLastTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"[0] | first"#, r#"0"#),
    Case::eq(r#"[1, 2, 3] | first"#, r#"1"#),
    Case::eq(r#"[1, 2, 3] | first 5"#, r#"[1, 2, 3]"#),
    Case::eq(r#"[1, 2, 3] | first 2"#, r#"[1, 2]"#),
    Case::eq(r#"[1, 2, 3] | first 1"#, r#"[1]"#),
    Case::eq(r#"[1, 2, 3] | first 0"#, r#"[]"#),
    Case::eq(r#"[] | first"#, r#"null"#),
    Case::eq(r#"'abc' | first"#, r#"'a'"#),
    Case::eq(r#"'abc' | first 2"#, r#"'ab'"#),
    Case::eq(r#"'abc'.first"#, r#"'a'"#),
    Case::eq(r#"'abc'.first 2"#, r#"'ab'"#),
    Case::eq(r#"'' | first"#, r#"null"#),
    Case::eq(
        r#"{ a: 1, b: 2, c: 3 } | first 2 | select --add --order=(.fields.name.join)"#,
        r#"{ a: 1, b: 2, order: 'ab' }"#,
    ),
    Case::eq(r#"{ a: 1, b: 2, c: 3 }.first 2"#, r#"{ a: 1, b: 2 }"#),
    Case::eq(r#"{ a: 1, b: 2, c: 3 } | first"#, r#"{ a: 1 }"#),
    Case::eq(r#"{} | first"#, r#"null"#),
    Case::setup(
        r#"class A (xs...) { def toList = xs }"#,
        &[
            Case::eq(r#"A 1 2 3 | first 2"#, r#"[1, 2]"#),
            Case::eq(r#"A.new | first"#, r#"null"#),
        ],
    ),
    Case::eq(r#"[1, 2, 3] | last"#, r#"3"#),
    Case::eq(r#"[1, 2, 3].last"#, r#"3"#),
    Case::eq(r#"[1, 2, 3] | last 2"#, r#"[2, 3]"#),
    Case::eq(r#"[1, 2, 3] | last 5"#, r#"[1, 2, 3]"#),
    Case::eq(r#"[] | last"#, r#"null"#),
    Case::eq(r#"'xyz' | last"#, r#"'z'"#),
    Case::eq(r#"'xyz' | last 2"#, r#"'yz'"#),
    Case::eq(r#"'xyz'.last"#, r#"'z'"#),
    Case::eq(r#"'xyz'.last 2"#, r#"'yz'"#),
    Case::eq(r#"'' | last"#, r#"null"#),
    Case::eq(
        r#"{ a: 1, b: 2, c: 3 } | last 2 | select --add --order=(.fields.name.join)"#,
        r#"{ b: 2, c: 3, order: 'bc' }"#,
    ),
    Case::eq(r#"{ a: 1, b: 2, c: 3 }.last 2"#, r#"{ b: 2, c: 3 }"#),
    Case::eq(r#"{ a: 1, b: 2, c: 3 } | last"#, r#"{ c: 3 }"#),
    Case::eq(r#"{} | last"#, r#"null"#),
    Case::setup(
        r#"class A (xs...) { def toList = xs }"#,
        &[
            Case::eq(r#"A 1 2 3 | last 2"#, r#"[2, 3]"#),
            Case::eq(r#"A.new | last"#, r#"null"#),
        ],
    ),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
