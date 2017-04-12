package com.github.mdr.mash.evaluator

class ArithmeticTest extends AbstractEvaluatorTest {

  // String multiplication
  "'xy' * 3" ==> "'xyxyxy'"
  "3 * 'xy'" ==> "'xyxyxy'"
  "'x' * 1" ==> "'x'"
  "'x' * 0" ==> "''"

  // List multiplication
  "[1, 2] * 3" ==> "[1, 2, 1, 2, 1, 2]"
  "3 * [1, 2]" ==> "[1, 2, 1, 2, 1, 2]"
  "[1] * 1" ==> "[1]"
  "[1] * 0" ==> "[]"

  // Tags
  "3 * 1.second | .tag" ==> "time.Seconds"
  "1.second * 3 | .tag" ==> "time.Seconds"

  // Precedence
  "1 * 2 + 3 * 4" ==> 14
  "3 * 100 / 3" ==> 100

  "10 / 10 / 10" ==> 0.1
  "1 - 2 - 3" ==> -4

  // String addition
  "'foo' + 42" ==> "'foo42'"
  "42 + 'foo'" ==> "'42foo'"

  // List addition
  "[1, 2, 3] + [4, 5]" ==> "[1, 2, 3, 4, 5]"

}
