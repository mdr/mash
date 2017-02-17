package com.github.mdr.mash.ns.net

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import com.github.mdr.mash.evaluator.{ Arguments, ToStringifier }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime._

object UrlEncodeFunction extends MashFunction("net.urlEncode") {

  object Params {
    val String = Parameter(
      nameOpt = Some("string"),
      summaryOpt = Some("String to URL encode"))
  }
  import Params._

  val params = ParameterModel(Seq(String))

  def apply(arguments: Arguments): MashString = {
    val boundParams = params.validate(arguments)
    val s = ToStringifier.stringify(boundParams(String))
    MashString(URLEncoder.encode(s, StandardCharsets.UTF_8.name))
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("URL encode the given value")
}