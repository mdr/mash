package com.github.mdr.mash.ns.net

import java.net.URLEncoder

import com.github.mdr.mash.evaluator.{ Arguments, ToStringifier }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.core.{ NumberClass, StringClass }
import com.github.mdr.mash.runtime._

object UrlEncodeFunction extends MashFunction("net.urlEncode") {

  object Params {
    val S = Parameter(
      nameOpt = Some("s"),
      summaryOpt = Some("String to URl encode"))
  }
  import Params._

  val params = ParameterModel(Seq(S))

  def apply(arguments: Arguments): MashString = {
    val boundParams = params.validate(arguments)
    val s = ToStringifier.stringify(boundParams(S))
    MashString(URLEncoder.encode(s, "utf-8"))
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("Url Encode the given value")
}