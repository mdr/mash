package com.github.mdr.mash.evaluator

class PatternsTest extends AbstractEvaluatorTest {

  "{ foo: 42, bar: 128 } | { foo } => foo + 1" ==> 43
  "{ foo: 42, bar: 128 } | { wibble } => wibble" ==> null
  "{ foo: 42, bar: 128 } | { foo, bar } => foo + bar" ==> 170
  "{ foo: 42, bar: 128 } | { foo: foofar } => foofar" ==> 42
  "{ foo: 42, bar: 128 } | { foo: _ } => 10" ==> 10
  "(_ => 42) 10" ==> 42
  "def foo { bar } = bar; foo { bar: 42 }" ==> 42
  "{ baz } = 42".shouldThrowAnException

}
