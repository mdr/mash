package com.github.mdr.mash.ns.os

import java.nio.file.{ Files, StandardCopyOption }

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.MashUnit
import org.apache.commons.io.FileUtils

object CopyFunction extends MashFunction("os.copy") {

  object Params {
    val SourcePaths = Parameter(
      nameOpt = Some("sourcePaths"),
      summaryOpt = Some("Paths to copy"),
      isVariadic = true,
      variadicAtLeastOne = true,
      variadicFlatten = true,
      descriptionOpt = Some("""There must be at least one path provided.
If there are multiple paths provided, the destination must be a directory.        
If a path is a directory, it and all its contents (recursively) will be copied to the destination."""))
    val Destination = Parameter(
      nameOpt = Some("destination"),
      summaryOpt = Some("Location to copy path(s)"),
      isLast = true,
      descriptionOpt = Some("""If the destination is a directory, the source paths will be copied into it.
If the destination is not a directory, only a single source path may be provided."""))
  }
  import Params._

  val params = ParameterModel(Seq(SourcePaths, Destination))

  def call(boundParams: BoundParams): MashUnit = {
    val sourcePaths = FunctionHelpers.interpretAsPaths(boundParams(SourcePaths))
    val destination = boundParams.validatePath(Destination)

    if (sourcePaths.isEmpty)
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
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Copy files and directories")

}