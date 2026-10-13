//! Port of `evaluator/ObjectMethodsTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(
        r#"{ foo: 3, bar: 4 }.transformValues (n => n * n)"#,
        r#"{ foo: 9, bar: 16 }"#,
    ),
    Case::eq(
        r#"{ foo: 3, bar: 4 }.transformValues (f v => "$f$v")"#,
        r#"{ foo: 'foo3', bar: 'bar4' }"#,
    ),
    Case::eq(
        r#"{ foo: 3, bar: 4 }.transformFields (.toUpper)"#,
        r#"{ FOO: 3, BAR: 4 }"#,
    ),
    Case::eq(
        r#"{ foo: 3, bar: 4 }.transformFields (f v => f.toUpper + v)"#,
        r#"{ FOO3: 3, BAR4: 4 }"#,
    ),
    Case::eq(
        r#"{ a: 10, bb: 20, ccc: 30 }.transformFields (.length)"#,
        r#"{ (1): 10, (2): 20, (3): 30 }"#,
    ),
    Case::eq(
        r#"{ foo: 42, bar: { baz1: 100, baz2: 200 } }.hoist 'bar'"#,
        r#"{ foo: 42, baz1: 100, baz2: 200 }"#,
    ),
    Case::eq(
        r#"{ foo: 42, (42): { baz1: 100, baz2: 200 } }.hoist 42"#,
        r#"{ foo: 42, baz1: 100, baz2: 200 }"#,
    ),
    Case::eq(
        r#"{ foo: 42, bar: [{ baz1: 100, baz2: 200 }, { baz1: 300, baz2: 400 }] }.hoist 'bar'"#,
        r#"[{ foo: 42, baz1: 100, baz2: 200 }, { foo: 42, baz1: 300, baz2: 400 }]"#,
    ),
    Case::eq(
        r#"{ foo: 42, bar: { baz1: 100, baz2: 200 } }.hoist 'bar' --prefix='bar_'"#,
        r#"{ foo: 42, bar_baz1: 100, bar_baz2: 200 }"#,
    ),
    Case::eq(r#"Object.merge []"#, r#"{}"#),
    Case::eq(r#"Object.merge { foo: 42 }"#, r#"{ foo: 42 }"#),
    Case::eq(r#"Object.merge { foo: 42 } { bar: 128 }"#, r#"{ foo: 42, bar: 128 }"#),
    Case::eq(r#"Object.merge { foo: 42 } { foo: 128 }"#, r#"{ foo: 128 }"#),
    Case::eq(
        r#"Object.merge { foo: 42 } { (42): 'foo' }"#,
        r#"{ foo: 42, (42): 'foo' }"#,
    ),
    Case::eq(
        r#"class A a; class B b; class C c; Object.merge [A 1, B 2, C 3] | .getClass.name"#,
        r#"'C'"#,
    ),
    Case::eq(r#"[['a', 1], ['b', 2]] | Object.fromPairs"#, r#"{ a: 1, b: 2 }"#),
    Case::eq(r#"[['a', 1], [2, 'b']] | Object.fromPairs"#, r#"{ a: 1, (2): 'b' }"#),
    Case::eq(
        r#"[['a', 1, 100], ['b', 2, 200]] | Object.fromPairs 0 2"#,
        r#"{ a: 100, b: 200 }"#,
    ),
    Case::eq(r#"[] | Object.fromPairs"#, r#"{}"#),
    Case::eq(
        r#"[{ name: 'a', value: 1 }, { name: 'b', value: 2 }] | Object.fromPairs"#,
        r#"{ a: 1, b: 2 }"#,
    ),
    Case::eq(
        r#"[{ name: 'a', value: 1, anotherValue: 100 }, { name: 'b', value: 2, anotherValue: 200 }] | Object.fromPairs 'name' 'anotherValue'"#,
        r#"{ a: 100, b: 200 }"#,
    ),
    Case::eq(r#"{}.withField 'foo' 42"#, r#"{ foo: 42 }"#),
    Case::eq(r#"{ foo: 42 }.withField 'bar' 256"#, r#"{ foo: 42, bar: 256 }"#),
    Case::eq(r#"{ foo: 42 }.withField 'foo' 256"#, r#"{ foo: 256 }"#),
    Case::eq(r#"{ foo: 42 }.withField 256 'foo'"#, r#"{ foo: 42, (256): 'foo' }"#),
    Case::eq(
        r#"{ a: 1, b: 2, c: 3, d: 4, e: 5, f: 6 }.withField 'c' 100 | .fields[2].name"#,
        r#"'c'"#,
    ),
    Case::eq(r#"{ foo: 42 }.get 'foo'"#, r#"42"#),
    Case::eq(r#"{ (42): 'foo'}.get 42"#, r#"'foo'"#),
    Case::eq(r#"{}.get 'nope'"#, r#"null"#),
    Case::eq(r#"{}.get 42"#, r#"null"#),
    Case::eq(r#"{}.get 'nope' --default=42"#, r#"42"#),
    Case::eq(r#"{ foo: 42 }.hasField 'foo'"#, r#"true"#),
    Case::eq(r#"{ (42): 'foo'}.hasField 42"#, r#"true"#),
    Case::eq(r#"{ foo: 42 }.hasField 'bar'"#, r#"false"#),
    Case::eq(r#"class Thing; Thing.new.unbless.getClass"#, r#"Object"#),
    Case::eq(r#"{ foo: 42 }.fields.name"#, r#"['foo']"#),
    Case::eq(r#"{ (42): 'foo'}.fields.name"#, r#"[42]"#),
    Case::throws(r#"42.fields"#),
    Case::eq(r#"{ foo: 42, bar: true }.values"#, r#"[42, true]"#),
    Case::eq(r#"{}.values"#, r#"[]"#),
    // New: only object classes can bless an object, as Class.bless (Scala made broken values)
    Case::throws(r#"{ a: 1 }.bless String"#),
    Case::throws(r#"{ a: 1 }.bless Number"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
