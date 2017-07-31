package com.github.mdr.mash.ns.os

import java.nio.file.{ Files, NoSuchFileException, Path }

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.os._
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashBoolean, MashList, MashObject }

object ListFilesFunction extends MashFunction("os.listFiles") {

  private val fileSystem: FileSystem = LinuxFileSystem

  object Params {
    val All = Parameter(
      nameOpt = Some("all"),
      summaryOpt = Some("Include files starting with a dot (default false)"),
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Recursive = Parameter(
      nameOpt = Some("recursive"),
      summaryOpt = Some("Recursively retrieve results from directories (default false)"),
      shortFlagOpt = Some('r'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Directory = Parameter(
      nameOpt = Some("directory"),
      summaryOpt = Some("List directories themselves, not their contents (default false)"),
      shortFlagOpt = Some('d'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Paths = Parameter(
      nameOpt = Some("paths"),
      summaryOpt = Some("Paths to list files"),
      isVariadic = true,
      variadicFlatten = true,
      defaultValueGeneratorOpt = Some(MashList.of(asPathString(""))),
      descriptionOpt = Some(
        s"""Paths can either be strings or ${PathSummaryClass.fullyQualifiedName} objects.
If a given path is a file, it will be included in the output.
If a given path is a directory, its children will be included, unless the
  directory parameter is true, then it will be included directly.
If no paths are provided, the default is the current working directory."""))
  }

  import Params._

  val params = ParameterModel(All, Recursive, Directory, Paths)

  def call(boundParams: BoundParams): MashList = {
    val ignoreDotFiles = boundParams(All).isFalsey
    val recursive = boundParams(Recursive).isTruthy
    val directory = boundParams(Directory).isTruthy
    val paths = boundParams.validatePaths(Paths)

    def listPath(path: Path): Seq[MashObject] =
      if (Files.isDirectory(path) && !directory)
        ChildrenFunction.getChildren(path, ignoreDotFiles = ignoreDotFiles, recursive = recursive)
      else {
        val summary =
          try
            fileSystem.getPathSummary(path)
          catch {
            case e: NoSuchFileException =>
              boundParams.throwInvalidArgument(Paths, s"No such file '${e.getFile}'")
          }
        Seq(PathSummaryClass.asMashObject(summary))
      }
    MashList(paths.flatMap(listPath))
  }

  override def typeInferenceStrategy = Seq(PathSummaryClass)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("List files")

  override def descriptionOpt = Some(
    s"""List files and directories, returning a sequence of ${PathSummaryClass.fullyQualifiedName} objects.
If no paths are supplied, the current directory is used as the default.""")

}