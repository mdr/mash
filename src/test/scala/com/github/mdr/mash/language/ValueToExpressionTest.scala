package com.github.mdr.mash.language

import com.github.mdr.mash.evaluator.TestEvaluator
import org.scalatest.{ FlatSpec, Matchers }

class ValueToExpressionTest extends FlatSpec with Matchers {

  "Converting a value to an expression" should "work" in {
    getExpression("null") shouldEqual Some("null")
    getExpression("true") shouldEqual Some("true")
    getExpression("1.23") shouldEqual Some("1.23")
    getExpression("'foo'") shouldEqual Some(""""foo"""")
    getExpression("[1, 2, 3]") shouldEqual Some("[1, 2, 3]")
    getExpression("{ foo: 42, bar: [1, 2, 3] }") shouldEqual Some("{ foo: 42, bar: [1, 2, 3] }")
    getExpression("""{ 10: 1, 'foo-bar': 2, ([1, 2, 3]): 3 }""") shouldEqual Some("""{ 10: 1, "foo-bar": 2, ([1, 2, 3]): 3 }""")
  }

  def getExpression(s: String) = ValueToExpression.getExpression(TestEvaluator.evaluate(s))

}
