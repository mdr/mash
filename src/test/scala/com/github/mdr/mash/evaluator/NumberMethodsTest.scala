package com.github.mdr.mash.evaluator

class NumberMethodsTest extends AbstractEvaluatorTest {

  // Number.to
  "1.to 5" ==> "[1, 2, 3, 4, 5]"
  "1.to 10 --step=2" ==> "[1, 3, 5, 7, 9]"
  "5.to 1 --step=-1" ==> "[5, 4, 3, 2, 1]"

  // Number.until
  "1.until 5" ==> "[1, 2, 3, 4]"
  "1.until 10 --step=2" ==> "[1, 3, 5, 7, 9]"
  "5.until 1 --step=-1" ==> "[5, 4, 3, 2]"

  // Number.times
  "a = 0; 5.times (a += 1); a" ==> 5
  "a = 0; (a += 1) | 5.times" ==> "[1, 2, 3, 4, 5]"

  // Number.power
  "2.power 8" ==> 256
}
