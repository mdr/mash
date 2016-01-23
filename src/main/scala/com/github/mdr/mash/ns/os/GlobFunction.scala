package com.github.mdr.mash.ns.os

import scala.collection.JavaConverters._
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.os._
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter

object GlobFunction extends MashFunction("os.glob") {

  private val fileSystem = LinuxFileSystem
  private val envInteractions = LinuxEnvironmentInteractions

  object Params {
    val Pattern = Parameter(
      name = "pattern",
      summary = "Pattern to match path names against")
  }
  import Params._

  val params = ParameterModel(Seq(Pattern))

  def apply(arguments: Arguments): MashList = {
    val boundParams = params.validate(arguments)
    val argument = boundParams.validateString(Pattern).s
    val pattern = new TildeExpander(envInteractions).expand(argument)
    MashList(fileSystem.glob(pattern).map(PathSummaryClass.asMashObject))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(PathSummaryClass)))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Return files matching a glob pattern"

}