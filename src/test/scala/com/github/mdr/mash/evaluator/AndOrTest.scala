package com.github.mdr.mash.evaluator

class AndOrTest extends AbstractEvaluatorTest {

  "true and true" ==> true
  "true and false" ==> false
  "false and true" ==> false
  "false and false" ==> false

  "true or true" ==> true
  "true or false" ==> true
  "false or true" ==> true
  "false or false" ==> false

  "true or null.bang" ==> true
  "false and null.bang" ==> false

  "{} or 42" ==> "42"
  "{ a: 1 } or 42" ==> "{ a: 1 }"

  "[] or 42" ==> "42"
  "[1] or 42" ==> "[1]"

  "0 or 42" ==> "42"
  "1 or 42" ==> "1"

  "null or 42" ==> "42"

}
