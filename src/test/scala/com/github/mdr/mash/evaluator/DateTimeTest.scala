package com.github.mdr.mash.evaluator

class DateTimeTest extends AbstractEvaluatorTest {

  "'29th June 2017, 18:36:44'.toDateTime | select --hour --minute --second" ==> "{ hour: 18, minute: 36, second: 44 }"

  "now.date" ==> "today"

}
