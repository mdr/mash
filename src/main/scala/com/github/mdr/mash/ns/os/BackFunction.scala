package com.github.mdr.mash.ns.os

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashUnit, MashValue }

object BackFunction extends MashFunction("os.back") {

  private val workingDirectoryStack = Singletons.workingDirectoryStack
  private val fileSystem = LinuxFileSystem

  val params = ParameterModel()

  def apply(boundParams: BoundParams): MashValue = {
    workingDirectoryStack.back() match {
      case Some(path) ⇒ fileSystem.chdir(path)
      case None ⇒ throw new EvaluatorException("No previous directory in history")
    }
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Go to the previous directory in the history")

}