package com.github.mdr.mash.ns.core

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashValue

object SafeFunction extends MashFunction("core.safe") {

  object Params {
    val Value = Parameter(
      nameOpt = Some("value"),
      isSafe = true)
  }

  import Params._

  val params = ParameterModel(Value)

  def call(boundParams: BoundParams): MashValue =
    boundParams(Value)

  override def summaryOpt = Some("Access the argument without triggering nullary evaluation")

  override def descriptionOpt = Some(
    """Examples:
      |<mash>
      |  safe ls               # Returns the function value, rather than executing it
      |  safe "string".reverse # Returns the bound method value, rather than executing it
      |</mash>
    """.stripMargin)

}
