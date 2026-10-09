//! Port of `evaluator/AssignmentTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"a = 42; a"#, r#"42"#),
    Case::eq(r#"a = {}; a.foo = 42; a.foo"#, r#"42"#),
    Case::eq(r#"a = [1, 2, 3]; a[1] = 42; a"#, r#"[1, 42, 3]"#),
    Case::throws(r#"a = [1, 2, 3]; a[3] = 42"#),
    Case::eq(r#"a = {}; a['foo'] = 42; a.foo"#, r#"42"#),
    Case::eq(r#"a = {}; a[true] = 42; a[true]"#, r#"42"#),
    Case::throws(r#"'foo'[0] = 'b'"#),
    Case::eq(r#"a = 0; a += 42; a"#, r#"42"#),
    Case::eq(r#"a = 42; a -= 42; a"#, r#"0"#),
    Case::eq(r#"a = 3; a *= 4; a"#, r#"12"#),
    Case::eq(r#"a = 15; a /= 5; a"#, r#"3"#),
    Case::eq(r#"a = { foo: 0 }; a.foo += 42; a"#, r#"{ foo: 42 }"#),
    Case::eq(r#"a = { (100): 0 }; a[100] += 42; a"#, r#"{ (100): 42 }"#),
    Case::throws(r#"a = { foo: 0 }; a.bar += 42"#),
    Case::eq(r#"a = [1, 0, 3]; a[1] += 42; a"#, r#"[1, 42, 3]"#),
    Case::throws(r#"a = [1, 0, 3]; a[3] += 42"#),
    Case::eq(r#"a = 42"#, r#"42"#),
    Case::eq(r#"a = 5; a += 10"#, r#"15"#),
    Case::eq(r#"{ foo } = { foo: 42 }"#, r#"{ foo: 42 }"#),
    Case::eq(r#"_ = 10"#, r#"10"#),
    Case::eq(r#"{ foo, bar } = { bar: 1, baz: 2, foo: 3 }; foo + bar"#, r#"4"#),
    Case::eq(r#"{ baz } = { foo: 42 }; baz"#, r#"null"#),
    Case::eq(r#"{ foo: { bar } } = { foo: { bar: 42 } }; bar"#, r#"42"#),
    Case::eq(r#"[a, b, c] = [1, 2, 3]; a + b + c"#, r#"6"#),
    Case::eq(r#"[a, b] = [1, 2, 3]; a + b"#, r#"3"#),
    Case::eq(r#"[a, b] = [1]; b"#, r#"null"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
