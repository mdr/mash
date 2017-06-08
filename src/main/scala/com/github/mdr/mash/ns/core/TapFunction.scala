package com.github.mdr.mash.ns.core

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashValue

object TapFunction extends MashFunction("core.tap") {

  object Params {
  val F = Parameter(
      nameOpt = Some("f"),
      summaryOpt = Some("Function to apply to the item"))
    val Value = Parameter(
      nameOpt = Some("value"),
      summaryOpt = Some("Value to tap"))
  }
  import Params._

  val params = ParameterModel(F, Value)

  def call(boundParams: BoundParams): MashValue = {
    val f = boundParams.validateFunction(F)
    val value = boundParams(Value)
    f.apply(value)
    value
  }

  override def typeInferenceStrategy = BooleanClass

  override def summaryOpt = Some("Apply a function to a value, ignore the result, and return the original value.")

}
