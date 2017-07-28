package com.github.mdr.mash.ns.mash

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.language.ValueToExpression
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.{ MashNull, MashString, MashValue }

object ExpressionForFunction extends MashFunction("mash.expressionFor") {

  object Params {
    val Value = Parameter(
      nameOpt = Some("value"),
      summaryOpt = Some("Value to turn into a Mash expression"))
  }

  import Params._

  val params = ParameterModel(Value)

  def call(boundParams: BoundParams): MashValue = {
    val value = boundParams(Value)
    ValueToExpression.getExpression(value).map(MashString(_)).getOrElse(MashNull)
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("Convert the given value into a Mash expression that evaluates to the same value")

}
