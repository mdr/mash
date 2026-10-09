//! Port of `evaluator/ObjectLiteralTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"{ 'foo': 42 }.foo"#, r#"42"#),
    Case::eq(r#"{ ('foo' + 'bar'): 42 }"#, r#"{ foobar: 42 }"#),
    Case::eq(r#"{ (42): 'foo' }[42]"#, r#"'foo'"#),
    Case::eq(r#"foo = 42; bar = 128; { foo, bar }"#, r#"{ foo: 42, bar: 128 }"#),
    Case::eq(r#"def pi = 3.14; { pi }"#, r#"{ pi: 3.14 }"#),
    Case::eq(r#"{ (def foo = 42): 42 }.values"#, r#"[42]"#),
    Case::eq(r#"def foo n = 42; { (foo): 42 }[foo]"#, r#"42"#),
    Case::eq(r#"{ (.foo): 42 }.values"#, r#"[42]"#),
    Case::eq(r#"{ (class A): 42 }.values"#, r#"[42]"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[
    // Cannot find member 'values' in value of type Object
    r#"{ (def foo = 42): 42 }.values"#,
    // Cannot find member 'values' in value of type Object
    r#"{ (.foo): 42 }.values"#,
    // Cannot find member 'values' in value of type Object
    r#"{ (class A): 42 }.values"#,
];
