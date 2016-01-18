package com.github.mdr.mash.ns.os

import java.nio.file.Files
import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.os._
import com.github.mdr.mash.ns.core.UnitClass

object DeleteFunction extends MashFunction("os.delete") {

  object Params {
    val Path = Parameter(
      name = "path",
      summary = "Path to delete")
    val Paths = Parameter(
      name = "paths",
      summary = "Additional paths to delete",
      isVariadic = true)
  }
  import Params._

  val params = ParameterModel(Seq(Params.Path, Paths))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val paths1 = FunctionHelpers.interpretAsPaths(boundParams(Params.Path))
    val paths2 = boundParams(Paths).asInstanceOf[Seq[_]].flatMap(FunctionHelpers.interpretAsPaths)
    val paths = paths1 ++ paths2
    for (path ← paths)
      if (Files.isDirectory(path))
        FileUtils.deleteDirectory(path.toFile)
      else
        Files.delete(path)
    ()
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Delete path(s)"

}