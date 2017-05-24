package com.github.mdr.mash.evaluator

class ImportTest extends AbstractEvaluatorTest {

  "import hash.sha256; sha256" ==> "sha256"
  "import hash._; sha256" ==> "sha256"
  "obj = { foo: 42 }; import obj.foo; foo" ==> 42
  "class A n { def inc = n += 1 }; a = A 0; import a._; inc; a.n" ==> 1
  "x = {}; import x._; where.getClass" ==> "Function" // Object.where is a 'shy' method
  "x = {}; import x._; isNull.getClass" ==> "Function" // Any.isNull is a 'shy' method
  "x = {}; import x.where; where.getClass" ==> "BoundMethod"
  "class A { @private def method = 42 }; a = A.new; import a.method" shouldThrowAnException

}
