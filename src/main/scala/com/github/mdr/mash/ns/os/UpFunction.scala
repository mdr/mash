package com.github.mdr.mash.ns.os

import java.nio.file.Paths

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashNumber, MashUnit }

object UpFunction extends MashFunction("os.up") {

  private val fileSystem = LinuxFileSystem
  private val workingDirectoryStack = Singletons.workingDirectoryStack

  object Params {
    val N = Parameter(
      nameOpt = Some("n"),
      summary = "Number of parent directories to move up (default 1)",
      defaultValueGeneratorOpt = Some(() ⇒ MashNumber(1)))
  }
  import Params._

  val params = ParameterModel(Seq(N))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val times = boundParams.validateInteger(N)
    workingDirectoryStack.push(fileSystem.pwd)
    for (n ← 1 to times)
      fileSystem.chdir(Paths.get(".."))
    MashUnit
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Change the current working directory to a parent (or ancestor)"

}
