package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, FunctionHelpers, MashMethod, ParameterModel }
import com.github.mdr.mash.os.CurrentDirectoryManager
import com.github.mdr.mash.os.CurrentDirectoryManager.{ NotADirectory, Success }
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashUnit, MashValue }

object ChangeDirectoryMethod extends MashMethod("changeDirectory") {

  private val currentDirectoryManager = CurrentDirectoryManager(LinuxFileSystem, Singletons.workingDirectoryStack)

  override def aliases = Seq("cd")

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashUnit = {
    val path = FunctionHelpers.interpretAsPath(target)
    currentDirectoryManager.changeDirectory(path) match {
      case Success       ⇒ MashUnit
      case NotADirectory ⇒ throw EvaluatorException(s"Could not change directory to '$path', not a directory")
    }
  }

  override def typeInferenceStrategy = Unit

  override def summaryOpt = Some("Change directory to this path")

}