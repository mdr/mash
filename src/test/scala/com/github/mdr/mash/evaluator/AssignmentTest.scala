package com.github.mdr.mash.evaluator

class AssignmentTest extends AbstractEvaluatorTest {

  "a = 42; a" ==> 42
  "a = {}; a['foo'] = 42; a.foo" ==> 42
  "a = [1, 2, 3]; a[1] = 42; a" ==> "[1, 42, 3]"

  "a = 0; a += 42; a" ==> 42
  "a = 42; a -= 42; a" ==> 0
  "a = 3; a *= 4; a" ==> 12
  "a = 15; a /= 5; a" ==> 3
  "a = { foo: 0 }; a.foo += 42; a" ==> "{ foo: 42 }"
  "a = [1, 0, 3]; a[1] += 42; a" ==> "[1, 42, 3]"

  "a = 42" ==> 42
  "a = 5; a += 10" ==> 15
  "_ = 10" ==> 10
  "{ foo } = { foo: 42 }" ==> "{ foo: 42 }"

}
