package com.github.mdr.mash.ns.os

import com.github.mdr.mash.WorkingDirectoryStack
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.runtime.{ MashUnit, MashValue }

case class BackFunction(workingDirectoryStack: WorkingDirectoryStack,
                        fileSystem: FileSystem)
  extends MashFunction("os.back") {

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashValue = {
    workingDirectoryStack.back() match {
      case Some(path) ⇒ fileSystem.chdir(path)
      case None       ⇒ throw EvaluatorException("No previous directory in history")
    }
    MashUnit
  }

  override def typeInferenceStrategy = Unit

  override def summaryOpt = Some("Go to the previous directory in the history")

}