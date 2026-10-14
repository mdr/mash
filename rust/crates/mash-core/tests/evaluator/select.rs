//! Port of `evaluator/SelectTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"{ foo: 42, bar: 24 } | select 'foo' "#, r#"{ foo: 42 }"#),
    Case::eq(r#"{ foo: 42, bar: 24 } | select --foo "#, r#"{ foo: 42 }"#),
    Case::eq(r#"{ foo: 42, bar: 24 } | select --foo=(_.foo) "#, r#"{ foo: 42 }"#),
    Case::eq(
        r#"{ foo: 42, bar: 24 } | select 'foo' --baz=(_.foo) 'bar' "#,
        r#"{ foo: 42, baz: 42, bar: 24 }"#,
    ),
    Case::eq(r#"[{ foo: 42, bar: 24 }] | select 'foo'"#, r#" [{ foo: 42 }] "#),
    Case::eq(r#"{ foo: 42 } | select --add --bar=(_.foo)"#, r#"{ foo: 42, bar: 42 }"#),
    Case::eq(
        r#"{ foo: 42, bar: 24 } | select 'foo' --bar"#,
        r#"{ foo: 42, bar: 24  }"#,
    ),
    Case::eq(r#"select 'foo' --target=[{ foo: 42 }]"#, r#" [{ foo: 42 }] "#),
    Case::eq(r#"select 'foo' --target={ foo: 42 }"#, r#"{ foo: 42 }"#),
    Case::eq(r#"select --foo --target={ foo: 42 }"#, r#"{ foo: 42 }"#),
    Case::eq(
        r#"select --add --bar=(_.foo) --target={ foo: 42 }"#,
        r#"{ foo: 42, bar: 42 }"#,
    ),
    Case::eq(
        r#"select -a --bar=(_.foo) --target={ foo: 42 }"#,
        r#"{ foo: 42, bar: 42 }"#,
    ),
    Case::eq(r#"[{ foo: 42, bar: 24 }].select 'foo'"#, r#"[{ foo: 42 }]"#),
    Case::eq(r#"class A; A.new | select --add --n=(_ => 42) | .getClass"#, r#"A"#),
    Case::eq(r#"class A; A.new | select --n=(_ => 42) | .getClass"#, r#"Object"#),
    // New: a member selected by name that an object doesn't have is null (Scala: an error), as in jq
    Case::eq(r#"select "a" [{ a: 1 }, { b: 2 }]"#, r#"[{ a: 1 }, { a: null }]"#),
    Case::eq(
        r#"[{ a: 1 }, { b: 2 }] | select --a --b"#,
        r#"[{ a: 1, b: null }, { a: null, b: 2 }]"#,
    ),
    Case::eq(r#"select "a" { b: 2 }"#, r#"{ a: null }"#),
    // A computed field is evaluated as written, so a missing member there is still an error
    Case::throws(r#"select --x=(_.nope) { a: 1 }"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
