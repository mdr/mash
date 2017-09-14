package com.github.mdr.mash.ns.core

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashBoolean

object NotFunction extends MashFunction("core.not") {

  object Params {
    val Item = Parameter(
      nameOpt = Some("item"),
      summaryOpt = Some("Item to negate"))
  }
  import Params._

  val params = ParameterModel(Item)

  def call(boundParams: BoundParams): MashBoolean = {
    MashBoolean(boundParams(Item).isFalsey)
  }

  override def typeInferenceStrategy = Boolean

  override def summaryOpt = Some("Logically negate the given item")

  override def descriptionOpt = Some("""The given argument is interpreted as truthy or falsey, then negated.

Examples:
<mash>
  not true  # false
  not false # true
  not 0     # true
  not 1     # false
</mash>""")

}
