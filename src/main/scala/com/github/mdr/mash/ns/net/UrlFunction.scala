package com.github.mdr.mash.ns.net

import java.net.{ URI, URISyntaxException }

import com.github.mdr.mash.evaluator.{ Arguments, ToStringifier }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashString

object UrlFunction extends MashFunction("net.url") {

  object Params {
    val Url = Parameter(
      nameOpt = Some("url"),
      summary = "String to interpret as a URL")
  }
  import Params._

  val params = ParameterModel(Seq(Url))

  def apply(arguments: Arguments): MashString = {
    val boundParams = params.validate(arguments)
    val s = ToStringifier.stringify(boundParams(Url))
    try
      new URI(s)
    catch {
      case e: URISyntaxException ⇒ boundParams.throwInvalidArgument(Url, e.getMessage)
    }
    MashString(s, UrlClass)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(StringClass taggedWith UrlClass)

  override def summaryOpt = Some("Interpret the given value as a URL")
}