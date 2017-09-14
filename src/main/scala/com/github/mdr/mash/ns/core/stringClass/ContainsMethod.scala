package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashString, MashValue }

object ContainsMethod extends MashMethod("contains") {

  object Params {
    val Substring = Parameter(
      nameOpt = Some("substring"),
      summaryOpt = Some("Substring to match"))
  }

  import Params._

  val params = ParameterModel(Substring)

  def call(target: MashValue, boundParams: BoundParams): MashBoolean = {
    val s = target.asInstanceOf[MashString].s
    val pattern = ToStringifier.stringify(boundParams(Substring))
    MashBoolean(s contains pattern)
  }

  override def typeInferenceStrategy = Boolean

  override def summaryOpt = Some("Test whether this string contains the given substring")

}
