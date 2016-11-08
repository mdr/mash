package com.github.mdr.mash.ns.os

import java.nio.file.Files
import java.nio.file.StandardCopyOption
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import java.nio.file.Path
import com.github.mdr.mash.runtime.MashUnit

object MoveFunction extends MashFunction("os.move") {

  object Params {
    val Paths = Parameter(
      name = "paths",
      summary = "Paths to move",
      descriptionOpt = Some("At least one path must provided."),
      isVariadic = true,
      variadicAtLeastOne = true)
    val Destination = Parameter(
      name = "destination",
      summary = "Location to move to",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Paths, Destination))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val sourcePaths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    val destination = boundParams.validatePath(Destination)
    if (sourcePaths.isEmpty)
      throw new EvaluatorException("Must have at least one source path")
    move(sourcePaths, destination)
    MashUnit
  }

  private def move(sourcePaths: Seq[Path], destination: Path) {
    val destinationIsDir = Files.isDirectory(destination)
    if (destinationIsDir) {
      for (source ← sourcePaths)
        if (Files.isDirectory(source))
          FileUtils.moveDirectoryToDirectory(source.toFile, destination.toFile, false)
        else
          FileUtils.moveFileToDirectory(source.toFile, destination.toFile, false)
    } else {
      if (sourcePaths.size > 1)
        throw new EvaluatorException("Multiple files can only be moved into a directory")
      for (source ← sourcePaths) {
        val newPath = source.resolveSibling(destination)
        Files.move(source, newPath, StandardCopyOption.REPLACE_EXISTING)
      }
    }

  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Move files or directories to a new location"

  override def descriptionOpt = Some("""If multiple paths are provided, the destination must be a directory.""")

}