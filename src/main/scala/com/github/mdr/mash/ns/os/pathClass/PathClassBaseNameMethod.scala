package com.github.mdr.mash.ns.os.pathClass

import org.apache.commons.io.FilenameUtils

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashValue

object PathClassBaseNameMethod extends MashMethod("baseName") {

  val params = ParameterModel()

  def apply(target: MashValue, arguments: Arguments): MashString = {
    params.validate(arguments)
    val name = FunctionHelpers.interpretAsPath(target).getFileName.toString
    MashString(FilenameUtils.getBaseName(name))
  }

  override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

  override def summary = "Name without extension"

}
