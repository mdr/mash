package com.github.mdr.mash.evaluator

class SelectTest extends AbstractEvaluatorTest {

  "{ foo: 42, bar: 24 } | select 'foo' " ==> "{ foo: 42 }"
  "{ foo: 42, bar: 24 } | select --foo " ==> "{ foo: 42 }"
  "{ foo: 42, bar: 24 } | select --foo=(_.foo) " ==> "{ foo: 42 }"
  "{ foo: 42, bar: 24 } | select 'foo' --baz=(_.foo) 'bar' " ==> "{ foo: 42, baz: 42, bar: 24 }"
  "[{ foo: 42, bar: 24 }] | select 'foo'" ==> " [{ foo: 42 }] "
  "{ foo: 42 } | select --add --bar=(_.foo)" ==> "{ foo: 42, bar: 42 }"
  "{ foo: 42, bar: 24 } | select 'foo' --bar" ==> "{ foo: 42, bar: 24  }"
  "select 'foo' --target=[{ foo: 42 }]" ==> " [{ foo: 42 }] "
  "select 'foo' --target={ foo: 42 }" ==> "{ foo: 42 }"
  "select --foo --target={ foo: 42 }" ==> "{ foo: 42 }"
  "select --add --bar=(_.foo) --target={ foo: 42 }" ==> "{ foo: 42, bar: 42 }"
  "select -a --bar=(_.foo) --target={ foo: 42 }" ==> "{ foo: 42, bar: 42 }"
  "[{ foo: 42, bar: 24 }].select 'foo'" ==> "[{ foo: 42 }]"
  "class A; A.new | select --add --n=(_ => 42) | .getClass" ==> "A"
  "class A; A.new | select --n=(_ => 42) | .getClass" ==> "Object"

}
