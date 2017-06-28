package com.github.mdr.mash.evaluator

class AssignmentTest extends AbstractEvaluatorTest {

  // Simple assignment
  "a = 42; a" ==> 42

  // Assignment to member expr
  "a = {}; a.foo = 42; a.foo" ==> 42

  // Assignment to lookup expr
  "a = [1, 2, 3]; a[1] = 42; a" ==> "[1, 42, 3]"
  "a = [1, 2, 3]; a[3] = 42".shouldThrowAnException

  "a = {}; a['foo'] = 42; a.foo" ==> 42
  "a = {}; a[true] = 42; a[true]" ==> 42

  "'foo'[0] = 'b'".shouldThrowAnException

  // Compound assignment
  "a = 0; a += 42; a" ==> 42
  "a = 42; a -= 42; a" ==> 0
  "a = 3; a *= 4; a" ==> 12
  "a = 15; a /= 5; a" ==> 3

  "a = { foo: 0 }; a.foo += 42; a" ==> "{ foo: 42 }"
  "a = { (100): 0 }; a[100] += 42; a" ==> "{ (100): 42 }"
  "a = { foo: 0 }; a.bar += 42".shouldThrowAnException

  "a = [1, 0, 3]; a[1] += 42; a" ==> "[1, 42, 3]"
  "a = [1, 0, 3]; a[3] += 42".shouldThrowAnException

  // Value of an assignment expression
  "a = 42" ==> 42
  "a = 5; a += 10" ==> 15
  "{ foo } = { foo: 42 }" ==> "{ foo: 42 }"
  "_ = 10" ==> 10

  // Patterns on LHS
  "{ foo, bar } = { bar: 1, baz: 2, foo: 3 }; foo + bar" ==> 4
  "{ baz } = { foo: 42 }; baz" ==> null
  "{ foo: { bar } } = { foo: { bar: 42 } }; bar" ==> 42

  "[a, b, c] = [1, 2, 3]; a + b + c" ==> 6
  "[a, b] = [1, 2, 3]; a + b" ==> 3
  "[a, b] = [1]; b" ==> null

}
