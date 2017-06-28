package com.github.mdr.mash.evaluator

class EqualityTest extends AbstractEvaluatorTest {

  "1 == 1" ==> true
  "1 == 2" ==> false

  "1 != 1" ==> false
  "1 != 2" ==> true

  "{ foo: 42, bar: 100 } == { bar: 100, foo: 42 }" ==> true
  "{ (42): 'foo', bar: 100 } == { bar: 100, (42): 'foo' }" ==> true
  "class Box n; Box 42 == { n: 42 }" ==> false
  "class Box n; b1 = Box 42; class Box n; b2 = Box 42; b1 == b2" ==> false

  "(class Box n) == (class Box n)" ==> false
  "class Box n; Box == Box" ==> true

}
