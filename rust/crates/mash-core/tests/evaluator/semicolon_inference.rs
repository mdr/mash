//! Port of `evaluator/SemicolonInferenceTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(
        r#"a = 1
    |b = a + 1
    |a + b"#,
        r#"3"#,
    ),
    Case::eq(
        r#"a = 1
    |b = a +
    |  1
    |a + b"#,
        r#"3"#,
    ),
    Case::eq(
        r#"def foo (arg1 = 10)
    |        (arg2 = 20) = 42
    |foo
  "#,
        r#"42"#,
    ),
    Case::eq(
        r#"def foo (arg1 = {
    |           a = 10
    |           b = 20
    |           a + b
    |        }) = arg1
    |foo
  "#,
        r#"30"#,
    ),
    Case::eq(
        r#"class A (arg1 = 10)
    |A.new.arg1
  "#,
        r#"10"#,
    ),
    Case::eq(
        r#"class A (arg1 = 10) (arg2 = 20) {
    |  def a = arg1 + arg2
    |  def b = a
    |}
    |A.new.b
  "#,
        r#"30"#,
    ),
    Case::eq(
        r#"class A
    |A.new.getClass.name
  "#,
        r#"'A'"#,
    ),
    Case::eq(
        r#"@attribute
    |class A {
    |  @attribute
    |  def a = 42
    |}
    |A.new.a
  "#,
        r#"42"#,
    ),
    Case::eq(
        r#"{ a: 1, b: 2 } | select --a
  'b'"#,
        r#"'b'"#,
    ),
    Case::eq(
        r#"[] | sort -d
  100"#,
        r#"100"#,
    ),
    Case::eq(
        r#"{ foo: { bar: 100 } }
      .foo
      .bar
  "#,
        r#"100"#,
    ),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
