//! Port of `evaluator/DocCommentsTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(
        r#"# Square a number
def square n = n * n
square.summary
  "#,
        r#"'Square a number'"#,
    ),
    Case::eq(
        r#"# Summary
# Description 1
# Description 2
# Description 3
def fun x = x
fun.description
  "#,
        r#"'Description 1`nDescription 2`nDescription 3'"#,
    ),
    Case::eq(
        r#"# Square a number
@private def square n = n * n
square.summary
  "#,
        r#"'Square a number'"#,
    ),
    Case::eq(
        r#"class A {
  # Do something
  @attribute
  def method = 42
}
(safe A.new.method).summary
  "#,
        r#"'Do something'"#,
    ),
    Case::eq(
        r#"class A {
  # Do something
  @(attribute "with argument")
  def method = 42
}
(safe A.new.method).summary
  "#,
        r#"'Do something'"#,
    ),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
