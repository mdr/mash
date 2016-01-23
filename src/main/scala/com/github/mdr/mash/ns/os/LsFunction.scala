package com.github.mdr.mash.ns.os

import java.nio.file.Files
import scala.collection.JavaConverters._
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.os._
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import java.nio.file.Path

object LsFunction extends MashFunction("os.ls") {

  private val fileSystem: FileSystem = LinuxFileSystem

  object Params {
    val Paths = Parameter(
      name = "paths",
      summary = "Paths to list files",
      isVariadic = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashList.of(asPathString(""))),
      descriptionOpt = Some("""Paths can either be strings or PathSummary objects. 
If a given path is a file, it will be included in the output. 
If a given path is a directory, its children will be included, unless the
   directory parameter is true, then it will be included directly. 
If no paths are provided, the default is the current working directory."""))
    val All = Parameter(
      name = "all",
      summary = "Include files starting with a dot (default false)",
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isFlagValueAllowed = false)
    val Recursive = Parameter(
      name = "recursive",
      summary = "Recursively retrieve results from directories (default false)",
      shortFlagOpt = Some('r'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isFlagValueAllowed = false)
    val Directory = Parameter(
      name = "directory",
      summary = "List directories themselves, not their contents (default false)",
      shortFlagOpt = Some('d'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isFlagValueAllowed = false)
  }

  import Params._

  val params = ParameterModel(Seq(Paths, All, Recursive, Directory))

  def apply(arguments: Arguments): MashList = {
    val boundParams = params.validate(arguments)
    val ignoreDotFiles = Truthiness.isFalsey(boundParams(All))
    val recursive = Truthiness.isTruthy(boundParams(Recursive))
    val directory = Truthiness.isTruthy(boundParams(Directory))
    val paths = boundParams(Paths).asInstanceOf[MashList].items.map(interpretAsPath)

    def listPath(path: Path): Seq[MashObject] =
      if (Files.isDirectory(path) && !directory)
        ChildrenFunction.getChildren(path, ignoreDotFiles = ignoreDotFiles, recursive = recursive)
      else
        Seq(PathSummaryClass.asMashObject(fileSystem.getPathSummary(path)))
    MashList(paths.flatMap(listPath))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(PathSummaryClass)))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "List files"

  override def descriptionOpt = Some("""List files and directories, returning a sequence of PathSummary objects. 
If no paths are supplied, the current directory is used as the default.""")

}