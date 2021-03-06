package com.github.mdr.mash.ns.os

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.MashList

object GlobFunction extends MashFunction("os.glob") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val Pattern = Parameter(
      nameOpt = Some("pattern"),
      summaryOpt = Some("Pattern to match path names against"))
  }
  import Params._

  val params = ParameterModel(Pattern)

  def call(boundParams: BoundParams): MashList = {
    val pattern = boundParams.validateString(Pattern).s
    MashList(fileSystem.glob(pattern).map(PathSummaryClass.asMashObject))
  }

  override def typeInferenceStrategy = Seq(PathSummaryClass)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Return files matching a glob pattern")

}