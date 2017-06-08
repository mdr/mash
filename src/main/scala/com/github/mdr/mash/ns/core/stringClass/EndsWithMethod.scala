package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashString, MashValue }

object EndsWithMethod extends MashMethod("endsWith") {

  object Params {
    val Suffix = Parameter(
      nameOpt = Some("suffix"),
      summaryOpt = Some("Suffix to test"))
  }

  import Params._

  val params = ParameterModel(Suffix)

  def call(target: MashValue, boundParams: BoundParams): MashBoolean = {
    val s = target.asInstanceOf[MashString]
    val pattern = boundParams(Suffix).asInstanceOf[MashString]
    MashBoolean(s.reverse.startsWith(pattern.reverse))
  }

  override def typeInferenceStrategy = BooleanClass

  override def summaryOpt = Some("Check if this string ends with another")

}
