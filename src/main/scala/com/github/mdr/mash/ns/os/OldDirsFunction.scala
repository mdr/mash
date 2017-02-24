package com.github.mdr.mash.ns.os

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, FunctionHelpers, MashFunction, ParameterModel }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashList

object OldDirsFunction extends MashFunction("os.oldDirs") {

  private val workingDirectoryStack = Singletons.workingDirectoryStack

  val params = ParameterModel()

  def apply(boundParams: BoundParams): MashList = {
    MashList(workingDirectoryStack.oldDirs.map(FunctionHelpers.asPathString))
  }

  override def typeInferenceStrategy = Type.Seq(StringClass taggedWith PathClass)

  override def summaryOpt = Some("Previous working directories")

}