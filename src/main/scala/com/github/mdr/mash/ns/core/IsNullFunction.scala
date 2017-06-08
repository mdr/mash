package com.github.mdr.mash.ns.core

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashBoolean

object IsNullFunction extends MashFunction("core.isNull") {

  object Params {
    val Value = Parameter(
      nameOpt = Some("value"),
      summaryOpt = Some("Value to test for nullness"))
  }
  import Params._

  val params = ParameterModel(Value)

  def call(boundParams: BoundParams): MashBoolean = {
    MashBoolean(boundParams(Value).isNull)
  }

  override def summaryOpt = Some("Check whether or not the given argument is null")

  override def descriptionOpt = Some("""Examples:
  isNull null # true
  isNull 0    # false""")

}
