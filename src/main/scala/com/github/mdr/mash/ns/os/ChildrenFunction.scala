package com.github.mdr.mash.ns.os

import java.nio.file.Path

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.os._
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashBoolean, MashList, MashObject, MashString }

object ChildrenFunction extends MashFunction("os.children") {

  private val fileSystem: FileSystem = LinuxFileSystem

  object Params {
    val Directory = Parameter(
      nameOpt = Some("directory"),
      summaryOpt = Some("Directory to inspect"),
      defaultValueGeneratorOpt = Some(() ⇒ MashString("", PathClass)))
    val IgnoreDotFiles = Parameter(
      nameOpt = Some("ignoreDotFiles"),
      summaryOpt = Some("Ignore files starting with a dot (.) (default false)"),
      shortFlagOpt = Some('i'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
    val Recursive = Parameter(
      nameOpt = Some("recursive"),
      summaryOpt = Some("Recursively retrieve results from subdirectories (default false)"),
      shortFlagOpt = Some('r'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Directory, IgnoreDotFiles, Recursive))

  def apply(boundParams: BoundParams): MashList = {
    val parentDir = boundParams.validatePath(Directory)
    if (!fileSystem.exists(parentDir))
      boundParams.throwInvalidArgument(Directory, s"'$parentDir' does not exist")
    if (!fileSystem.isDirectory(parentDir))
      boundParams.throwInvalidArgument(Directory, s"'$parentDir' is not a directory")
    val ignoreDotFiles = boundParams(IgnoreDotFiles).isTruthy
    val recursive = boundParams(Recursive).isTruthy
    MashList(getChildren(parentDir, ignoreDotFiles = ignoreDotFiles, recursive = recursive))
  }

  def getChildren(parentDir: Path, ignoreDotFiles: Boolean, recursive: Boolean): Seq[MashObject] = {
    val paths = fileSystem.getChildren(parentDir, ignoreDotFiles = ignoreDotFiles, recursive = recursive)
    paths.map(PathSummaryClass.asMashObject)
  }

  override def typeInferenceStrategy = Type.Seq(Type.Instance(PathSummaryClass))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Return the children of a directory")

  override def descriptionOpt = Some("""Return sequence of PathSummary objects representing the children of the given path.
If no path is supplied, the current directory is used as the default.""")

}