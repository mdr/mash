package com.github.mdr.mash.evaluator

class GetClassMethodTest extends AbstractEvaluatorTest {

  "1.getClass" shouldEvaluateTo "core.Number"
  "true.getClass" shouldEvaluateTo "core.Boolean"
  "'foo'.getClass" shouldEvaluateTo "core.String"
  "now.getClass" shouldEvaluateTo "time.DateTime"
  "now.date.getClass" shouldEvaluateTo "time.Date"
  "().getClass" shouldEvaluateTo "core.Unit"
  "null.getClass" shouldEvaluateTo "core.Null"
  "{}.getClass" shouldEvaluateTo "core.Object"
  "[].getClass" shouldEvaluateTo "collections.List"
  "identity.getClass" shouldEvaluateTo "core.Function"
  "'foo'.startsWith.getClass" shouldEvaluateTo "core.BoundMethod"
  "1.getClass.getClass" shouldEvaluateTo "core.Class"

}