package com.github.mdr.mash.ns.os

import java.nio.file.Files
import java.nio.file.Path
import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.Posix
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.BytesClass
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import java.nio.file.StandardCopyOption
import com.github.mdr.mash.ns.core.UnitClass
object MvFunction extends MashFunction("os.mv") {

  private val Paths = "paths"
  private val Destination = "destination"

  val params = ParameterModel(Seq(
    Parameter(
      name = Paths,
      summary = "Paths to move",
      isVariadic = true),
    Parameter(
      name = Destination,
      summary = "Location to move path(s) to",
      isLast = true)))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val sourcePaths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    val destination = FunctionHelpers.interpretAsPath(boundParams(Destination))
    if (sourcePaths.size == 0)
      throw new EvaluatorException("Must have at least one source path")
    val destinationIsDir = Files.isDirectory(destination)
    if (destinationIsDir) {
      for (source ← sourcePaths)
        if (Files.isDirectory(source))
          FileUtils.moveDirectoryToDirectory(source.toFile, destination.toFile, false)
        else
          FileUtils.moveFileToDirectory(source.toFile, destination.toFile, false)
    } else {
      if (sourcePaths.size > 1)
        throw new EvaluatorException("Multiple files can only be moved into a directory, but the provided destination was not")
      for (source ← sourcePaths) {
        val newPath = source.resolveSibling(destination)
        Files.move(source, newPath, StandardCopyOption.REPLACE_EXISTING)
      }
    }
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Move path(s)"

}