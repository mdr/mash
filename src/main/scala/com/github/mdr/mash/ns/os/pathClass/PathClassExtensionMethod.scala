package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashValue

object PathClassExtensionMethod extends MashMethod("extension") {

  val params = ParameterModel()

  def apply(target: MashValue, arguments: Arguments): MashValue = {
    params.validate(arguments)
    val name = FunctionHelpers.interpretAsPath(target).getFileName.toString
    if (name contains ".")
      MashString(name.reverse.takeWhile(_ != '.').reverse)
    else
      MashNull
  }

  override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

  override def summary = "File extension, if any, else null"

}