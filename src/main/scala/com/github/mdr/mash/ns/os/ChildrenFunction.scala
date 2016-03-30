package com.github.mdr.mash.ns.os

import java.nio.file.Path
import scala.collection.immutable.ListMap
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.BytesClass
import com.github.mdr.mash.os._
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter

object ChildrenFunction extends MashFunction("os.children") {

  private val fileSystem: FileSystem = LinuxFileSystem

  object Params {
    val Directory = Parameter(
      name = "directory",
      summary = "Directory to inspect",
      defaultValueGeneratorOpt = Some(() ⇒ MashString("", PathClass)))
    val IgnoreDotFiles = Parameter(
      name = "ignoreDotFiles",
      summary = "Ignore files starting with a dot (.) (default false)",
      shortFlagOpt = Some('i'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isBooleanFlag = true)
    val Recursive = Parameter(
      name = "recursive",
      summary = "Recursively retrieve results from subdirectories (default false)",
      shortFlagOpt = Some('r'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Directory, IgnoreDotFiles, Recursive))

  def apply(arguments: Arguments): MashList = {
    val boundParams = params.validate(arguments)
    val parentDir = boundParams.validatePath(Directory)
    if (!fileSystem.exists(parentDir))
      boundParams.throwInvalidArgument(Directory, s"'$parentDir' does not exist")
    if (!fileSystem.isDirectory(parentDir))
      boundParams.throwInvalidArgument(Directory, s"'$parentDir' is not a directory")
    val ignoreDotFiles = Truthiness.isTruthy(boundParams(IgnoreDotFiles))
    val recursive = Truthiness.isTruthy(boundParams(Recursive))
    MashList(getChildren(parentDir, ignoreDotFiles = ignoreDotFiles, recursive = recursive))
  }

  def getChildren(parentDir: Path, ignoreDotFiles: Boolean, recursive: Boolean): Seq[MashObject] = {
    val paths = fileSystem.getChildren(parentDir, ignoreDotFiles = ignoreDotFiles, recursive = recursive)
    paths.map(PathSummaryClass.asMashObject)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(PathSummaryClass)))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Return the children of a directory"

  override def descriptionOpt = Some("""Return sequence of PathSummary objects representing the children of the given path.
If no path is supplied, the current directory is used as the default.""")

}