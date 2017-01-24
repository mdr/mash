package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashBoolean

object NotFunction extends MashFunction("core.not") {

  object Params {
    val Item = Parameter(
      nameOpt = Some("item"),
      summaryOpt = Some("Item to negate"))
  }
  import Params._

  val params = ParameterModel(Seq(Item))

  def apply(arguments: Arguments): MashBoolean = {
    val boundParams = params.validate(arguments)
    MashBoolean(boundParams(Item).isFalsey)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(BooleanClass)

  override def summaryOpt = Some("Logically negate the given item")

  override def descriptionOpt = Some("""The given argument is interpreted as truthy or falsey, then negated.

Examples:
  not true  # false
  not false # true
  not 0     # true
  not 1     # false""")

}
