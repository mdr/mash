package com.github.mdr.mash.ns.os

import java.nio.file.Files
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference._
import com.github.mdr.mash.os._
import java.nio.file.StandardCopyOption
import com.github.mdr.mash.ns.core.UnitClass

object CopyFunction extends MashFunction("os.copy") {

  object Params {
    val SourcePaths = Parameter(
      name = "sourcePaths",
      summary = "Paths to copy",
      isVariadic = true,
      variadicAtLeastOne = true,
      descriptionOpt = Some("""There must be at least one path provided.
If there are multiple paths provided, the destination must be a directory.        
If a path is a directory, it and all its contents (recursively) will be copied to the destination."""))
    val Destination = Parameter(
      name = "destination",
      summary = "Location to copy path(s)",
      isLast = true,
      descriptionOpt = Some("""If the destination is a directory, the source paths will be copied into it.
If the destination is not a directory, only a single source path may be provided."""))
  }
  import Params._

  val params = ParameterModel(Seq(SourcePaths, Destination))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val sourcePaths = FunctionHelpers.interpretAsPaths(boundParams(SourcePaths))
    val destination = boundParams.validatePath(Destination)

    if (sourcePaths.size == 0)
      throw new EvaluatorException("Must have at least one source path")

    val destinationIsDir = Files.isDirectory(destination)
    if (destinationIsDir) {
      for (source ← sourcePaths)
        if (Files.isDirectory(source))
          FileUtils.copyDirectoryToDirectory(source.toFile, destination.toFile)
        else
          FileUtils.copyFileToDirectory(source.toFile, destination.toFile)
    } else {
      if (sourcePaths.size > 1)
        throw new EvaluatorException("Multiple source paths were provided, but the destination was not a directory")
      for (source ← sourcePaths)
        if (Files.isDirectory(source))
          if (Files.exists(destination))
            throw new EvaluatorException("Destination already exists")
          else
            FileUtils.copyDirectory(source.toFile, destination.toFile)
        else {
          if (Files.exists(destination) && Files.isDirectory(destination))
            throw new EvaluatorException("Destination already exists, and is a directory")
          else
            Files.copy(source, destination, StandardCopyOption.REPLACE_EXISTING)
        }

    }
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Copy files and directories"

}