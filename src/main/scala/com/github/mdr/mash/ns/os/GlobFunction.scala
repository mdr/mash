package com.github.mdr.mash.ns.os

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.os.linux.{ LinuxEnvironmentInteractions, LinuxFileSystem }
import com.github.mdr.mash.runtime.MashList

object GlobFunction extends MashFunction("os.glob") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val Pattern = Parameter(
      nameOpt = Some("pattern"),
      summaryOpt = Some("Pattern to match path names against"))
  }
  import Params._

  val params = ParameterModel(Seq(Pattern))

  def apply(arguments: Arguments): MashList = {
    val boundParams = params.validate(arguments)
    val pattern = boundParams.validateString(Pattern).s
    MashList(fileSystem.glob(pattern).map(PathSummaryClass.asMashObject))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Seq(PathSummaryClass))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Return files matching a glob pattern")

}