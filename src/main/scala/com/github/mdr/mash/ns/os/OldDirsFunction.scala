package com.github.mdr.mash.ns.os

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass

object OldDirsFunction extends MashFunction("os.oldDirs") {

  private val workingDirectoryStack = Singletons.workingDirectoryStack

  val params = ParameterModel()

  def apply(arguments: Arguments): MashList = {
    val boundParams = params.validate(arguments)
    MashList(workingDirectoryStack.oldDirs.map(FunctionHelpers.asPathString))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(StringClass taggedWith PathClass))

  override def summary = "Previous working directories"

}