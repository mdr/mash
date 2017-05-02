package com.github.mdr.mash.evaluator

class DefaultArgumentsTest extends AbstractEvaluatorTest {

  "def foo (x = 42) = x + 1; foo" ==> 43
  "def foo (x = 42) = x + 1; foo 100" ==> 101
  "def fun ({ foo } = { foo: 42 }) = foo + 1; fun" ==> 43
  "def fun ({ foo } = { foo: 42 }) = foo + 1; fun { foo: 100 }" ==> 101
  "def mkList (xs... = [1, 2, 3]) = xs; mkList" ==> "[1, 2, 3]"
  "def mkList (xs... = [1, 2, 3]) = xs; mkList --xs=[]" ==> "[]"
  "def mkList (xs... = [1, 2, 3]) = xs; mkList --xs=[4, 5, 6]" ==> "[4, 5, 6]"
  "def mkList (xs... = null) = xs; mkList" ==> null

  // For methods
  "class Foo wibble { def get (n = wibble) = n }; Foo 100 | .get" ==> 100
  "class Foo wibble { def get (n = this) = n }; Foo 100 | .get.wibble" ==> 100
  "class Foo { def foo (n = bar) = n; def bar = 100 }; Foo.new.foo" ==> 100

  // Default argument scope bug
  "init = 42; a = ((v = init) => init = v); a" ==> 42
  "init = 42; a = (def mkFun (v = init) = init = v); a" ==> 42

  // Default arguments are not reused
  "def foo (x = [1]) = { y = x[0]; x[0] = 100; y }; foo; foo" ==> 1

}
