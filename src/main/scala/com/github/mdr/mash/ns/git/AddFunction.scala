package com.github.mdr.mash.ns.git

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ FunctionHelpers, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantTypeInferenceStrategy, TypedArguments }
import com.github.mdr.mash.runtime.MashUnit

object AddFunction extends MashFunction("git.add") {

  object Params {
    val Paths = Parameter(
      nameOpt = Some("paths"),
      summary = "Add paths to the index",
      isVariadic = true,
      variadicAtLeastOne = true)
  }
  import Params._

  val params = ParameterModel(Seq(Paths))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val paths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    GitHelper.withGit { git ⇒
      val cmd = git.add
      for (path ← paths)
        cmd.addFilepattern(path.toString)
      cmd.call()
    }
    MashUnit
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summaryOpt = Some("Record changes to the repository")

}