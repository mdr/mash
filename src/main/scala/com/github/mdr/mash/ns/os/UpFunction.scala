package com.github.mdr.mash.ns.os

import java.nio.file.Paths

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashNumber, MashUnit }

object UpFunction extends MashFunction("os.up") {

  private val fileSystem = LinuxFileSystem
  private val workingDirectoryStack = Singletons.workingDirectoryStack

  object Params {
    val N = Parameter(
      nameOpt = Some("n"),
      summaryOpt = Some("Number of parent directories to move up (default 1)"),
      defaultValueGeneratorOpt = Some(MashNumber(1)))
  }
  import Params._

  val params = ParameterModel(Seq(N))

  def apply(boundParams: BoundParams): MashUnit = {
    val times = boundParams.validateInteger(N)
    for (n ← 1 to times)
      fileSystem.chdir(Paths.get(".."))
    workingDirectoryStack.push(fileSystem.pwd)
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Change the current working directory to a parent (or ancestor)")

}
