package com.github.mdr.mash.ns.os

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashUnit, MashValue }

object ForwardFunction extends MashFunction("os.forward") {

  private val workingDirectoryStack = Singletons.workingDirectoryStack
  private val fileSystem = LinuxFileSystem

  val params = ParameterModel()

  def call(boundParams: BoundParams): MashValue = {
    workingDirectoryStack.forward() match {
      case Some(path) ⇒ fileSystem.chdir(path)
      case None       ⇒ throw new EvaluatorException("No next directory in history")
    }
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Go forwards to the next directory in the history")

}