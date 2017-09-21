package com.github.mdr.mash.ns.html

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashString
import org.apache.commons.text.StringEscapeUtils

object EscapeFunction extends MashFunction("html.escape") {

  object Params {
    val String = Parameter(
      nameOpt = Some("string"),
      summaryOpt = Some("String to escape"))
  }

  import Params._

  val params = ParameterModel(String)

  def call(boundParams: BoundParams): MashString = {
    val s = boundParams.validateString(String).s
    MashString(StringEscapeUtils.escapeHtml4(s))
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("Escape the characters using HTML entities")

}