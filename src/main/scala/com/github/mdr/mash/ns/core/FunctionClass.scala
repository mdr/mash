package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.help.{ FunctionHelpClass, HelpFunction }
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object FunctionClass extends MashClass("core.Function") {

  override val methods = Seq(
    HelpMethod)

  object HelpMethod extends MashMethod("help") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      params.validate(arguments)
      HelpFunction.getHelp(target.asInstanceOf[MashFunction])
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(FunctionHelpClass)

    override def summary = "Help documentation for this function"
  }

  override def summary = "A function"

  override def parentOpt = Some(AnyClass)

}