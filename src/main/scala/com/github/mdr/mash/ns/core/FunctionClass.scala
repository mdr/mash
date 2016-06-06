package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.ns.core.help.ParameterHelpClass
import com.github.mdr.mash.functions.Parameter
import scala.collection.immutable.ListMap
import com.github.mdr.mash.ns.core.help.HelpFunction
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashValue

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

}