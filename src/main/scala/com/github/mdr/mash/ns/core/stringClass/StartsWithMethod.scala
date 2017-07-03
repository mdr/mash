package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashString, MashValue }

object StartsWithMethod extends MashMethod("startsWith") {

  object Params {
    val Prefix = Parameter(
      nameOpt = Some("prefix"),
      summaryOpt = Some("Prefix to test"))
  }

  import Params._

  val params = ParameterModel(Prefix)

  def call(target: MashValue, boundParams: BoundParams): MashBoolean = {
    val s = target.asInstanceOf[MashString]
    val pattern = boundParams.validateString(Prefix)
    MashBoolean(s startsWith pattern)
  }

  override def typeInferenceStrategy = BooleanClass

  override def summaryOpt = Some("Check if this string starts with another")

}
