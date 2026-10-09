//! Port of `evaluator/ObjectArithmeticTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"{ foo: 42 } + { bar: 100 }"#, r#"{ foo: 42, bar: 100 }"#),
    Case::eq(r#"{ foo: 42 } + { }"#, r#"{ foo: 42 }"#),
    Case::eq(r#"{ foo: 42 } + { foo: 100 }"#, r#"{ foo: 100 }"#),
    Case::eq(r#"class Box n; Box 5 + { foo: 42 } | .getClass.name"#, r#"'Box'"#),
    Case::eq(r#"class Box n; { foo: 42 } + Box 5 | .getClass.name"#, r#"'Box'"#),
    Case::eq(r#"class A a; class B b; A 'a' + B 'b' | .getClass.name"#, r#"'B'"#),
    Case::eq(r#"{ foo: 42, bar: 100 } - 'foo'"#, r#"{ bar: 100 }"#),
    Case::eq(r#"{ foo: 42 } - 'bar'"#, r#"{ foo: 42 }"#),
    Case::eq(r#"{ (42): 'foo' } - 42"#, r#"{}"#),
    Case::eq(r#"{ foo: 42 } - []"#, r#"{ foo: 42 }"#),
    Case::eq(r#"{ foo: 1, bar: 2, baz: 3 } - ['baz', 'foo']"#, r#"{ bar: 2 }"#),
    Case::eq(
        r#"{ foo: 1, bar: 2, baz: 3 } - ['baz', 'foo', 'notInObject']"#,
        r#"{ bar: 2 }"#,
    ),
    Case::eq(r#"{ (42): 'foo' } - [42]"#, r#"{}"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
