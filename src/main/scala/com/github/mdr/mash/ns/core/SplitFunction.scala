package com.github.mdr.mash.ns.core

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.stringClass.SplitMethod
import com.github.mdr.mash.runtime.MashList

object SplitFunction extends MashFunction("core.split") {

  object Params {

    val String = Parameter(
      nameOpt = Some("string"),
      summaryOpt = Some("String to split"))

  }

  import Params._
  import SplitMethod.Params._

  val params = ParameterModel(Regex, Separator, String)

  def call(boundParams: BoundParams): MashList = {
    val string = boundParams.validateString(String)
    val regex = boundParams(Regex).isTruthy
    val separator = SplitMethod.getSeparator(boundParams, Separator, regex)
    SplitMethod.split(string, separator)
  }

  override def typeInferenceStrategy = (inferencer, arguments) => {
    val argBindings = params.bindTypes(arguments)
    val stringTypeOpt = argBindings.getType(String)
    stringTypeOpt orElse Some(Type.Instance(StringClass)) map (_.seq)
  }

  override def summaryOpt = Some("Split this string into a sequence of substrings using a separator")

}