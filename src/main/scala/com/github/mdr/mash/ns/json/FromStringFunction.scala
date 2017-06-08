package com.github.mdr.mash.ns.json

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime.MashValue

object FromStringFunction extends MashFunction("json.fromString") {

  object Params {
    val String = Parameter(
      nameOpt = Some("string"),
      summaryOpt = Some("String to parse as JSON"))
  }
  import Params._

  val params = ParameterModel(String)

  def call(boundParams: BoundParams): MashValue = {
    val s = boundParams.validateString(String).s
    ReadFunction.parseJson(s)
  }

  override def typeInferenceStrategy = AnyClass

  override def summaryOpt = Some("Read the given string and parse it as JSON")
}