package com.github.mdr.mash.ns.git

import scala.annotation.migration
import scala.collection.JavaConverters.asScalaSetConverter
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
import com.github.mdr.mash.inference.Type.unitToType
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.runtime.MashUnit

object StageFunction extends MashFunction("git.stage") {

  object Params {
    val Paths = Parameter(
      name = "paths",
      summary = "Stage the given paths",
      isVariadic = true)
    val All = Parameter(
      name = "all",
      summary = "Stage all unstaged files (default false)",
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
      val status = git.status.call()
      val missing = status.getMissing.asScala
      val filesToStage = if (all) getUnstagedFiles else paths.map(_.toString)
      val (toDelete, toAdd) = filesToStage.partition(missing.contains)

      if (toAdd.nonEmpty) {
        val addCommand = git.add
        for (path ← toAdd)
          addCommand.addFilepattern(path)
        addCommand.call()
      }

      if (toDelete.nonEmpty) {
        val rmCommand = git.rm.setCached(true)
        for (path ← toDelete)
          rmCommand.addFilepattern(path)
        rmCommand.call()
      }
    }
    MashUnit
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Paths ⇒ Try(CompletionSpec.Items(getUnstagedFiles)) getOrElse CompletionSpec.File
    }

  private def getUnstagedFiles: Seq[String] = {
    val status = GitHelper.withGit { _.status.call() }
    (status.getUntracked.asScala ++
      status.getModified.asScala ++
      status.getMissing.asScala ++
      status.getConflicting.asScala).toSeq.distinct
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Stage files"

}