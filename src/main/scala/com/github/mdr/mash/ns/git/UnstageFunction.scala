package com.github.mdr.mash.ns.git

import scala.collection.JavaConverters._
import scala.util.Try
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.runtime.MashUnit

object UnstageFunction extends MashFunction("git.unstage") {

  object Params {
    val Paths = Parameter(
      name = "paths",
      summary = "Unstage the given paths",
      isVariadic = true)
    val All = Parameter(
      name = "all",
      summary = "Unstage all staged files (default false)",
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Paths, All))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val paths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    val all = Truthiness.isTruthy(boundParams(All))
    if (paths.isEmpty && !all)
      throw new EvaluatorException(s"Must provide either '$Paths' or '$All'")
    GitHelper.withGit { git ⇒
      val cmd = git.reset
      if (all)
        for (file ← getStagedFiles)
          cmd.addPath(file)
      for (path ← paths)
        cmd.addPath(path.toString)
      cmd.call()
    }
    MashUnit
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Paths ⇒ Try(CompletionSpec.Items(getStagedFiles)) getOrElse CompletionSpec.File
    }

  private def getStagedFiles: Seq[String] = {
    val status = GitHelper.withGit { _.status.call() }
    (status.getAdded.asScala ++ status.getChanged.asScala ++ status.getRemoved.asScala).toSeq
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Unstage files"

}