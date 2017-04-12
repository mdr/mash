package com.github.mdr.mash.evaluator

class ComparisonTest extends AbstractEvaluatorTest {

  "1 < 2" ==> true
  "1 < 1" ==> false
  "1 < 0" ==> false

  "1 <= 2" ==> true
  "1 <= 1" ==> true
  "1 <= 0" ==> false

  "1 > 2" ==> false
  "1 > 1" ==> false
  "1 > 0" ==> true

  "1 >= 2" ==> false
  "1 >= 1" ==> true
  "1 >= 0" ==> true

  "[] < [1]" ==> true
  "[1] < [1, 2]" ==> true

  "1 == 1" ==> true
  "1 == 2" ==> false

  "1 != 1" ==> false
  "1 != 2" ==> true

  // Chained comparisons
  "1 < 2 < 3" ==> true
  "1 <= 2 < 3" ==> true
  "1 < 100 < 3" ==> false
  "1 < 2 < -100" ==> false
  "1 < -10 < 100" ==> false
  "a = 0; -100 < (a = a + 1; a) < 100; a" ==> 1
  "a = 0; (a += 1; 100) < (a += 1; -100) < (a += 1; -1000); a" ==> 3 // should evaluate all expressions

  // date/time comparisons
  "now > 3.days.ago" ==> true
  "now < 3.days.ago" ==> false
  "now.date > 3.days.ago.date" ==> true
  "now.date < 3.days.ago.date" ==> false

  // comparisons with null
  "null < 1" ==> false
  "null <= 1" ==> false
  "null > 1" ==> false
  "null >= 1" ==> false
  "1 < null" ==> false
  "1 <= null" ==> false
  "1 > null" ==> false
  "1 >= null" ==> false
  "null < null" ==> false
  "null < null < null" ==> false

  // comparisons across types
  "() < true < 1 < 'foo' < now < reverse < [].sortBy < [] < {} < String" ==> true

  // object comparisons
  "{ foo: 1 } < { foo: 2 }" ==> true
  "{ foo: 1 } < { foo: 1, bar: 1 }" ==> true
  "{ foo: 1, bar: 1 } < { foo: 1, bar: 2 }" ==> true
  "{ foo: 1, bar: 1 } < { foo: 1, car: 1 }" ==> true
  "{ a: 1, b: 1 } < { b: 1, a: 1 }" ==> true

  // boolean comparisons
  "false < true" ==> true
  "false < false" ==> false
  "true < false" ==> false
  "true < true" ==> false
  "false <= false" ==> true
  "true <= true" ==> true

  // date/time comparisons
  "now > 1.day.ago" ==> true

}
