package com.github.mdr.mash.evaluator

class ClassMethodTest extends AbstractEvaluatorTest {

  "1.class" shouldEvaluateTo "ns.core.Number"
  "true.class" shouldEvaluateTo "ns.core.Boolean"
  "'foo'.class" shouldEvaluateTo "ns.core.String"
  "now.class" shouldEvaluateTo "ns.time.DateTime"
  "now.date.class" shouldEvaluateTo "ns.time.Date"
  "().class" shouldEvaluateTo "ns.core.Unit"
  "null.class" shouldEvaluateTo "ns.core.Null"
  "{}.class" shouldEvaluateTo "ns.core.Object"
  "[].class" shouldEvaluateTo "ns.collections.Seq"
  "identity.class" shouldEvaluateTo "ns.core.Function"
  "'foo'.startsWith.class" shouldEvaluateTo "ns.core.BoundMethod"
  "1.class.class" shouldEvaluateTo "ns.core.Class"

}