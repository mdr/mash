package com.github.mdr.mash.ns.git

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashList
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type.unitToType
import com.github.mdr.mash.inference.TypedArguments
import scala.collection.JavaConverters._

object UnstageFunction extends MashFunction("git.unstage") {

  object Params {
    val Paths = Parameter(
      name = "paths",
      summary = "Unstage the given paths",
      isVariadic = true,
      variadicAtLeastOne = true)
  }
  import Params._

  val params = ParameterModel(Seq(Paths))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val paths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    GitHelper.withGit { git ⇒
      val cmd = git.reset
      for (path ← paths)
        cmd.addPath(path.toString)
      cmd.call()
    }
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = {
    val status = GitHelper.withGit { _.status.call() }
    val stagedFiles =
      status.getAdded.asScala ++ status.getChanged.asScala ++ status.getRemoved.asScala
    Seq(CompletionSpec.Items(stagedFiles.toSeq))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Unstage files"

}