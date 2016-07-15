package com.github.mdr.mash.evaluator

class ClassMethodTest extends AbstractEvaluatorTest {

  "1.class" shouldEvaluateTo "core.Number"
  "true.class" shouldEvaluateTo "core.Boolean"
  "'foo'.class" shouldEvaluateTo "core.String"
  "now.class" shouldEvaluateTo "time.DateTime"
  "now.date.class" shouldEvaluateTo "time.Date"
  "().class" shouldEvaluateTo "core.Unit"
  "null.class" shouldEvaluateTo "core.Null"
  "{}.class" shouldEvaluateTo "core.Object"
  "[].class" shouldEvaluateTo "collections.List"
  "identity.class" shouldEvaluateTo "core.Function"
  "'foo'.startsWith.class" shouldEvaluateTo "core.BoundMethod"
  "1.class.class" shouldEvaluateTo "core.Class"

}