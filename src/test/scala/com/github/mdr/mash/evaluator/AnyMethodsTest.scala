package com.github.mdr.mash.evaluator

class AnyMethodsTest extends AbstractEvaluatorTest {

  // Any.in
  "42.in [1, 2, 3]" ==> false
  "2.in [1, 2, 3]" ==> true
  "null.in [1, 2, 3]" ==> false
  "null.in [null]" ==> true

  // Any.toString
  "null.toString" ==> " 'null' "
  "2.toString" ==> " '2' "
  "().toString" ==> "''"
  "true.toString" ==> " 'true' "
  "[1, 2, 3].toString" ==> "'[1, 2, 3]'"
  """ "foo".toString.tag """ ==> """ "foo".tag """
  "{ foo: 42 }" ==> "{ foo: 42 }"

  // Any.isA
  "42.isA Number" ==> true
  "42.isA Any" ==> true
  "42.isA String" ==> false
  "42.isA Object" ==> false
  
  "null.isA Object" ==> false
  "null.isA Null" ==> true
  "null.isA Any" ==> true
  
  "class A; A.new.isA A" ==> true
  "class A; A.new.isA Object" ==> true
  "class A; A.new.isA Any" ==> true
  "class A; {}.isA A" ==> false
  "class A; A.isA A" ==> false
  "class A; A.isA Class" ==> true
  
  "3.days.isA 3.days.tag" ==> true

}
