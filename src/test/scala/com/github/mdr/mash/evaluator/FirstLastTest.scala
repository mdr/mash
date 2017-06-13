package com.github.mdr.mash.evaluator

class FirstLastTest extends AbstractEvaluatorTest {

  // first
  "[0] | first" ==> 0
  "[1, 2, 3] | first" ==> 1
  "[1, 2, 3] | first 5" ==> "[1, 2, 3]"
  "[1, 2, 3] | first 2" ==> "[1, 2]"
  "[1, 2, 3] | first 1" ==> "[1]"
  "[1, 2, 3] | first 0" ==> "[]"
  "[] | first" ==> null

  "'abc' | first" ==> "'a'"
  "'abc' | first 2" ==> "'ab'"
  "'abc'.first" ==> "'a'"
  "'abc'.first 2" ==> "'ab'"
  "'' | first" ==> null

  "{ a: 1, b: 2, c: 3 } | first 2 | select --add --order=(.fields.name.join)" ==> "{ a: 1, b: 2, order: 'ab' }"
  "{ a: 1, b: 2, c: 3 }.first 2" ==> "{ a: 1, b: 2 }"
  "{ a: 1, b: 2, c: 3 } | first" ==> "{ a: 1 }"
  "{} | first" ==> null

  havingFirstRun("class A (xs...) { def toList = xs }") { implicit env ⇒
    "A 1 2 3 | first 2" ==> "[1, 2]"
    "A.new | first" ==> null
  }

  // last
  "[1, 2, 3] | last" ==> 3
  "[1, 2, 3].last" ==> 3
  "[1, 2, 3] | last 2" ==> "[2, 3]"
  "[1, 2, 3] | last 5" ==> "[1, 2, 3]"
  "[] | last" ==> null

  "'xyz' | last" ==> "'z'"
  "'xyz' | last 2" ==> "'yz'"
  "'xyz'.last" ==> "'z'"
  "'xyz'.last 2" ==> "'yz'"
  "'' | last" ==> null

  "{ a: 1, b: 2, c: 3 } | last 2 | select --add --order=(.fields.name.join)" ==> "{ b: 2, c: 3, order: 'bc' }"
  "{ a: 1, b: 2, c: 3 }.last 2" ==> "{ b: 2, c: 3 }"
  "{ a: 1, b: 2, c: 3 } | last" ==> "{ c: 3 }"
  "{} | last" ==> null

  havingFirstRun("class A (xs...) { def toList = xs }") { implicit env ⇒
    "A 1 2 3 | last 2" ==> "[2, 3]"
    "A.new | last" ==> null
  }

}
