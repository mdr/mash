package com.github.mdr.mash.evaluator

class PrivateMethodsTest extends AbstractEvaluatorTest {

  "class A { @private def a = 42 }; A.new.a" shouldThrowAnException

  "class A { @private def a = 42 }; A.new['a']" shouldThrowAnException

  "class A { @private def a = 42 }; 'a' A.new" shouldThrowAnException

  "class A { @private def a = 42 }; [A.new].a" shouldThrowAnException

  "class A { @private def a = 42; def b = a }; A.new.b" ==> 42
  "class A { @private def a = 42; def b = this.a }; A.new.b" ==> 42
  "class A { @private def a = 42; def b = this['a'].invoke }; A.new.b" ==> 42

}
