package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashBoolean, MashList, MashValue }

object CreateDirectoryFunction extends MashFunction("os.createDirectory") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val CreateIntermediates = Parameter(
      nameOpt = Some("createIntermediates"),
      summaryOpt = Some("Create intermediate directories as required (default false)"),
      shortFlagOpt = Some('c'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
    val Paths = Parameter(
      nameOpt = Some("paths"),
      summaryOpt = Some("Paths to create directories at"),
      isVariadic = true,
      variadicAtLeastOne = true)
  }
  import Params._

  val params = ParameterModel(Seq(CreateIntermediates, Paths))

  def apply(boundParams: BoundParams): MashValue = {
    val paths = interpretAsPaths(boundParams(Paths))
    val createIntermediates = boundParams(CreateIntermediates).isTruthy
    val resultPaths =
      for (path ← paths)
        yield fileSystem.createDirectory(path, createIntermediates)
    resultPaths.map(asPathString) match {
      case Seq(resultPath) ⇒ resultPath
      case paths           ⇒ MashList(paths)
    }
  }

  override def typeInferenceStrategy = StringClass taggedWith PathClass

  override def summaryOpt = Some("Create a new directory")

}