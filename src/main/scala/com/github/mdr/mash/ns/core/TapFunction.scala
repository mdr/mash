package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashValue

object TapFunction extends MashFunction("core.tap") {

  object Params {
  val F = Parameter(
      nameOpt = Some("f"),
      summary = "Function to apply to the item")
    val Value = Parameter(
      nameOpt = Some("value"),
      summary = "Value to tap")
  }
  import Params._

  val params = ParameterModel(Seq(F, Value))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val f = boundParams.validateFunction(F)
    val value = boundParams(Value)
    f.apply(value)
    value
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(BooleanClass)

  override def summary = "Apply a function to a value, ignore the result, and return the original value."

}
