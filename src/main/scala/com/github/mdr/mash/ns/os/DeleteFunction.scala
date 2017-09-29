package com.github.mdr.mash.ns.os

import java.nio.file.Files

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.MashUnit
import org.apache.commons.io.FileUtils

object DeleteFunction extends MashFunction("os.delete") {

  object Params {
    val Paths = Parameter(
      nameOpt = Some("paths"),
      summaryOpt = Some("Paths to delete"),
      isVariadic = true,
      variadicAtLeastOne = true,
      variadicFlatten = true)
  }

  import Params._

  val params = ParameterModel(Paths)

  def call(boundParams: BoundParams): MashUnit = {
    val paths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    if (paths.isEmpty)
      throw EvaluatorException("Must provide at least one path to delete")
    val deletionOrder = paths.sortBy(_.toAbsolutePath.toString).reverse
    for (path ← deletionOrder)
      if (Files.isDirectory(path))
        FileUtils.deleteDirectory(path.toFile)
      else
        Files.delete(path)
    MashUnit
  }

  override def typeInferenceStrategy = Unit

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Delete files and directories")

}