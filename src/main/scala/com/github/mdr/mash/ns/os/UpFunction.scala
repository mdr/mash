package com.github.mdr.mash.ns.os

import scala.collection.JavaConverters._
import com.github.mdr.mash.Posix
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.os._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.Singletons
import com.github.mdr.mash.ns.core.UnitClass

object UpFunction extends MashFunction("os.up") {

  private val fileSystem = LinuxFileSystem
  private val workingDirectoryStack = Singletons.workingDirectoryStack

  object Params {
    val N = Parameter(
      name = "n",
      summary = "Number of parent directories to move up",
      defaultValueGeneratorOpt = Some(() ⇒ MashNumber(1)))
  }
  import Params._

  val params = ParameterModel(Seq(N))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val times = boundParams.validateInteger(N)
    workingDirectoryStack.push(fileSystem.pwd)
    for (n ← 1 to times)
      Posix.posix.chdir("..")
    ()
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Change the current working directory to a parent (or ancestor)"

}
