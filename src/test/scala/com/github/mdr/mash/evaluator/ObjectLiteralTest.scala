package com.github.mdr.mash.evaluator

class ObjectLiteralTest extends AbstractEvaluatorTest {

  "{ 'foo': 42 }.foo" ==> 42
  "{ ('foo' + 'bar'): 42 }" ==> "{ foobar: 42 }"
  "{ (42): 'foo' }[42]" ==> "'foo'"

  "foo = 42; bar = 128; { foo, bar }" ==> "{ foo: 42, bar: 128 }"

  "def pi = 3.14; { pi }" ==> "{ pi: 3.14 }"

}
