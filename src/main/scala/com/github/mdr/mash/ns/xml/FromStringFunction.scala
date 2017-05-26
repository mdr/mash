package com.github.mdr.mash.ns.xml

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.MashValue

object FromStringFunction extends MashFunction("xml.fromString") {

  object Params {
    val String = Parameter(
      nameOpt = Some("string"),
      summaryOpt = Some("String to parse as XML"))
  }
  import Params._

  val params = ParameterModel(Seq(String))

  def call(boundParams: BoundParams): MashValue = {
    val xml = boundParams.validateString(String).s
    ReadFunction.fromString(xml)
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt = Some("Read the given string and parse it as XML")
}