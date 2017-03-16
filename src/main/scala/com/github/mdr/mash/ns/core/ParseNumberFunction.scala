package com.github.mdr.mash.ns.core

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashNumber

object ParseNumberFunction extends MashFunction("core.parseNumber") {

  object Params {
    val String = Parameter(
      nameOpt = Some("string"),
      summaryOpt = Some("String to parse as a number"))
  }

  val params = ParameterModel(Seq(Params.String))

  def apply(boundParams: BoundParams): MashNumber = {
    val s = boundParams.validateString(Params.String)
    MashNumber(s.s.toDouble)
  }

  override def typeInferenceStrategy = NumberClass

  override def summaryOpt = Some("Parse the given string as a number")

  override def descriptionOpt = Some("""Examples:
  parseNumber "42" # 42""")
}
