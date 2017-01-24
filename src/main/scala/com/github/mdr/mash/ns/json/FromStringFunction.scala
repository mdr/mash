package com.github.mdr.mash.ns.json

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantTypeInferenceStrategy, Type }
import com.github.mdr.mash.runtime.MashValue

object FromStringFunction extends MashFunction("json.fromString") {

  object Params {
    val String = Parameter(
      nameOpt = Some("string"),
      summaryOpt = Some("String to parse as JSON"))
  }
  import Params._

  val params = ParameterModel(Seq(String))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val s = boundParams.validateString(String).s
    FromFileFunction.parseJson(s)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Any)

  override def summaryOpt = Some("Read the given string and parse it as JSON")
}