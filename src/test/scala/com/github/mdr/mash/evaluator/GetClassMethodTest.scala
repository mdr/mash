package com.github.mdr.mash.evaluator

class GetClassMethodTest extends AbstractEvaluatorTest {

  "1.getClass" ==> "core.Number"
  "true.getClass" ==> "core.Boolean"
  "'foo'.getClass" ==> "core.String"
  "now.getClass" ==> "time.DateTime"
  "now.date.getClass" ==> "time.Date"
  "().getClass" ==> "core.Unit"
  "null.getClass" ==> "core.Null"
  "{}.getClass" ==> "core.Object"
  "[].getClass" ==> "collections.List"
  "identity.getClass" ==> "core.Function"
  "'foo'.startsWith.getClass" ==> "core.BoundMethod"
  "1.getClass.getClass" ==> "core.Class"

}