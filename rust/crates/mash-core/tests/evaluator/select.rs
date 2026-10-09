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
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
