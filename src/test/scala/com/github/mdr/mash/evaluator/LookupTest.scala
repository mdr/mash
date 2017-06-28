package com.github.mdr.mash.evaluator

class LookupTest extends AbstractEvaluatorTest {

  "[1, 2, 3][0]" ==> 1
  "[1, 2, 3][3]".shouldThrowAnException
  "[1, 2, 3][-1]" ==> 3
  "[1, 2, 3] | _[0]" ==> 1

  "'bar'[0]" ==> "'b'"
  "'bar'[3]".shouldThrowAnException

  "{ foo: 42 }['foo'] " ==> 42
  "{ foo: 42 }['bar'] ".shouldThrowAnException

  "{ (42): 'foo' }[42]" ==> "'foo'"
  "{ (true): 'foo' }[true]" ==> "'foo'"
  "{ ({ bar: 100 }): 'foo' }[{ bar: 100 }]" ==> "'foo'"

}
