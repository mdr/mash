package com.github.mdr.mash.evaluator

class PatternsTest extends AbstractEvaluatorTest {

  "{ foo: 42, bar: 128 } | { foo } => foo + 1" ==> 43
  "{ foo: 42, bar: 128 } | { wibble } => wibble" ==> null
  "{ foo: 42, bar: 128 } | { foo, bar } => foo + bar" ==> 170
  "{ foo, bar } = { bar: 1, baz: 2, foo: 3 }; foo + bar" ==> 4
  "{ baz } = { foo: 42 }; baz" ==> null
  "{ foo: { bar } } = { foo: { bar: 42 } }; bar" ==> 42
  "{ foo: 42, bar: 128 } | { foo: foofar } => foofar" ==> 42
  "{ foo: 42, bar: 128 } | { foo: _ } => 10" ==> 10
  "(_ => 42) 10" ==> 42
  "def foo { bar } = bar; foo { bar: 42 }" ==> 42
  "[a, b, c] = [1, 2, 3]; a + b + c" ==> 6
  "[a, b] = [1, 2, 3]; a + b" ==> 3
  "[a, b] = [1]; b" ==> null
  "{ baz } = 42" shouldThrowAnException

}
