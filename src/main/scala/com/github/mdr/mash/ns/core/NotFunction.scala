package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference._

object NotFunction extends MashFunction("core.not") {

  object Params {
    val Item = Parameter(
      name = "item",
      summary = "Item to negate")
  }
  import Params._

  val params = ParameterModel(Seq(Item))

  def apply(arguments: Arguments): Boolean = {
    val boundParams = params.validate(arguments)
    Truthiness.isFalsey(boundParams(Item))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(BooleanClass)

  override def summary = "Logically negate the given item"

  override def descriptionOpt = Some("""The given argument is interpreted as truthy or falsey, then negated.

Examples:
  not true  # false
  not false # true
  not 0     # true
  not 1     # false""")

}
