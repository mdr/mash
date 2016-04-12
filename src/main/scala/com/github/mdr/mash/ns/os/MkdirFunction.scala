package com.github.mdr.mash.ns.os

import java.nio.file.Files
import scala.collection.JavaConverters._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os._
import com.github.mdr.mash.os.linux.LinuxFileSystem

object MkdirFunction extends MashFunction("os.mkdir") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val CreateIntermediates = Parameter(
      name = "createIntermediates",
      summary = "Create intermediate directories as required (default false)",
      shortFlagOpt = Some('p'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isBooleanFlag = true)
    val Directory = Parameter(
      name = "directory",
      summary = "Path to directory to create")
  }
  import Params._

  val params = ParameterModel(Seq(CreateIntermediates, Directory))

  def apply(arguments: Arguments): MashString = {
    val boundParams = params.validate(arguments)
    val path = boundParams.validatePath(Directory)
    val createIntermediates = Truthiness.isTruthy(boundParams(CreateIntermediates))
    val resultPath = fileSystem.createDirectory(path, createIntermediates)
    asPathString(resultPath)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

  override def summary = "Create directory at this path"

}