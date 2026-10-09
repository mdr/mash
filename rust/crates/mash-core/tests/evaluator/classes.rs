//! Port of `evaluator/ClassesTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"class Point x y; Point.new 3 4 | [.x, .y]"#, r#"[3, 4]"#),
    Case::eq(r#"class Point x y; Point 3 4 | [.x, .y]"#, r#"[3, 4]"#),
    Case::eq(r#"class Point x y { def sum = x + y }; Point 3 4 | .sum"#, r#"7"#),
    Case::eq(
        r#"class Point x y { def sum = this.x + this.y }; Point 3 4 | .sum"#,
        r#"7"#,
    ),
    Case::eq(
        r#"class Point x y { def sum = x + y; def sumSquared = sum * sum }; Point 3 4 | .sumSquared"#,
        r#"49"#,
    ),
    Case::eq(
        r#"class Box n { def update n = this.n = n }; b = Box 0; b.update 10; b.n"#,
        r#"10"#,
    ),
    Case::eq(
        r#"class Box n { def increment = n += 1 }; box = Box 10; box.increment; box.n"#,
        r#"11"#,
    ),
    Case::eq(
        r#"class Box n { def increment = this['n'] += 1 }; box = Box 10; box.increment; box.n"#,
        r#"11"#,
    ),
    Case::eq(
        r#"class Outer o1 {
       def outer o2 = {
         class Inner i1 {
           def inner i2 = o1 + o2 + i1 + i2
         }
         Inner 1 | .inner 2
       }
     }
     Outer 3 | .outer 4
  "#,
        r#"10"#,
    ),
    Case::eq(r#"class Thing x { def x = 100 }; Thing 42 | .x"#, r#"42"#),
    Case::eq(
        r#"class Thing { def x = 100; def y = { x = 42; x } }; Thing.new.y"#,
        r#"42"#,
    ),
    Case::eq(r#"class Thing; Thing.new.getClass.name"#, r#"'Thing'"#),
    Case::eq(r#"class Thing { }; Thing.new.getClass.name"#, r#"'Thing'"#),
    Case::eq(
        r#"class Point x y { def add = x + y }; [Point 1 2, Point 3 4].add"#,
        r#"[3, 7]"#,
    ),
    Case::eq(r#"[Object, Object].merge { foo: 42 }"#, r#"[{ foo: 42 }, { foo: 42 }]"#),
    Case::eq(
        r#"class Foo { def getFields = this.fields }; Foo.new.getFields"#,
        r#"[]"#,
    ),
    Case::eq(
        r#"class Foo { def getFields = this.toString }; Foo.new.getFields"#,
        r#"'{}'"#,
    ),
    Case::eq(r#"(class A { def foo = 42 }) | .new.foo"#, r#"42"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
