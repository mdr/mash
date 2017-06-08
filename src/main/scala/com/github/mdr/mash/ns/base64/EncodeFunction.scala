package com.github.mdr.mash.ns.base64

import java.util.Base64

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashString

object EncodeFunction extends MashFunction("base64.encode") {

  object Params {
    val String = Parameter(
      nameOpt = Some("string"),
      summaryOpt = Some("String to encode in base 64"))
  }
  import Params._

  val params = ParameterModel(String)

  def call(boundParams: BoundParams): MashString = {
    val s = ToStringifier.stringify(boundParams(String))
    MashString(new String(Base64.getEncoder.encode(s.getBytes)))
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("Base 64 encode the given value")
}
