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
    val Path = Parameter(
      name = "path",
      summary = "Directory to inspect",
      defaultValueGeneratorOpt = Some(() ⇒ MashString("", Some(PathClass))))
    val IgnoreDotFiles = Parameter(
      name = "ignoreDotFiles",
      summary = "Ignore files starting with a dot (.) (default false)",
      shortFlagOpt = Some('i'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isFlagValueAllowed = false)
    val Recursive = Parameter(
      name = "recursive",
      summary = "Recursively retrieve results from subdirectories (default false)",
      shortFlagOpt = Some('r'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isFlagValueAllowed = false)
  }

  val params = ParameterModel(Seq(Params.Path, Params.IgnoreDotFiles, Params.Recursive))

  def apply(arguments: Arguments): Seq[MashObject] = {
    val boundParams = params.validate(arguments)
    val ignoreDotFiles = Truthiness.isTruthy(boundParams(Params.IgnoreDotFiles))
    val recursive = Truthiness.isTruthy(boundParams(Params.Recursive))
    val parentDir = interpretAsPath(boundParams(Params.Path))
    getChildren(parentDir, ignoreDotFiles, recursive)
  }

  def getChildren(parentDir: Path, ignoreDotFiles: Boolean, recursive: Boolean): Seq[MashObject] =
    fileSystem.getChildren(parentDir, ignoreDotFiles, recursive).map(PathSummaryClass.asMashObject)

  override def typeInferenceStrategy =
    ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(PathSummaryClass)))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    Seq(CompletionSpec.File)

  override def summary = "Return the children of a directory"

}