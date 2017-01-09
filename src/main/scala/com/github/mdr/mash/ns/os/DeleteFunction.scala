package com.github.mdr.mash.ns.os

import java.nio.file.Files

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashUnit
import org.apache.commons.io.FileUtils

object DeleteFunction extends MashFunction("os.delete") {

  object Params {
    val Paths = Parameter(
      nameOpt = Some("paths"),
      summary = "Paths to delete",
      isVariadic = true,
      variadicAtLeastOne = true)
  }
  import Params._

  val params = ParameterModel(Seq(Paths))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val paths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    if (paths.isEmpty)
      throw new EvaluatorException("Must provide at least one path to delete")
    for (path ← paths)
      if (Files.isDirectory(path))
        FileUtils.deleteDirectory(path.toFile)
      else
        Files.delete(path)
    MashUnit
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Delete files and directories"

}