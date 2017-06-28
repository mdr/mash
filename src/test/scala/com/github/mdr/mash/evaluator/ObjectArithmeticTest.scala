package com.github.mdr.mash.evaluator

class ObjectArithmeticTest extends AbstractEvaluatorTest {

  // object addition
  "{ foo: 42 } + { bar: 100 }" ==> "{ foo: 42, bar: 100 }"
  "{ foo: 42 } + { }" ==> "{ foo: 42 }"
  "{ foo: 42 } + { foo: 100 }" ==> "{ foo: 100 }"
  "class Box n; Box 5 + { foo: 42 } | .getClass.name" ==> "'Box'"
  "class Box n; { foo: 42 } + Box 5 | .getClass.name" ==> "'Box'"
  "class A a; class B b; A 'a' + B 'b' | .getClass.name" ==> "'B'"

  // object field subtraction
  "{ foo: 42, bar: 100 } - 'foo'" ==> "{ bar: 100 }"
  "{ foo: 42 } - 'bar'" ==> "{ foo: 42 }"
  "{ (42): 'foo' } - 42" ==> "{}"

  "{ foo: 42 } - []" ==> "{ foo: 42 }"
  "{ foo: 1, bar: 2, baz: 3 } - ['baz', 'foo']" ==> "{ bar: 2 }"
  "{ foo: 1, bar: 2, baz: 3 } - ['baz', 'foo', 'notInObject']" ==> "{ bar: 2 }"
  "{ (42): 'foo' } - [42]" ==> "{}"

}
