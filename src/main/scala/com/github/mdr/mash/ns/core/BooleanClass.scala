package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel

object BooleanClass extends MashClass("core.Boolean") {

  override val methods = Seq(
    NotMethod)

  object NotMethod extends MashMethod("not") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): Boolean = {
      params.validate(arguments)
      !target.asInstanceOf[Boolean]
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Negate this boolean"

    override def descriptionOpt = Some("""Examples:
    true.negate  # false
    false.negate # true""")

  }

  override def summary = "Boolean values true and false"
}