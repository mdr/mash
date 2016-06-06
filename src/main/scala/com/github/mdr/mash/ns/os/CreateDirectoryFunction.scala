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
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashBoolean

object CreateDirectoryFunction extends MashFunction("os.createDirectory") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val CreateIntermediates = Parameter(
      name = "createIntermediates",
      summary = "Create intermediate directories as required (default false)",
      shortFlagOpt = Some('c'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
    val Paths = Parameter(
      name = "paths",
      summary = "Paths to create directories at",
      isVariadic = true,
      variadicAtLeastOne = true)
  }
  import Params._

  val params = ParameterModel(Seq(CreateIntermediates, Paths))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    val paths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    val createIntermediates = Truthiness.isTruthy(boundParams(CreateIntermediates))
    val resultPaths =
      for (path ← paths)
        yield fileSystem.createDirectory(path, createIntermediates)
    resultPaths.map(asPathString) match {
      case Seq(resultPath) ⇒ resultPath
      case paths           ⇒ MashList(paths)
    }
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

  override def summary = "Create a new directory"

}