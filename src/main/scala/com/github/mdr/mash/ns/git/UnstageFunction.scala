package com.github.mdr.mash.ns.git

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashUnit }

import scala.collection.JavaConverters._
import scala.util.Try

object UnstageFunction extends MashFunction("git.unstage") {

  object Params {
    val All = Parameter(
      nameOpt = Some("all"),
      summaryOpt = Some("Unstage all staged files (default false)"),
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Paths = Parameter(
      nameOpt = Some("paths"),
      summaryOpt = Some("Unstage the given paths"),
      isVariadic = true,
      variadicFlatten = true)
  }
  import Params._

  val params = ParameterModel(All, Paths)

  def call(boundParams: BoundParams): MashUnit = {
    val paths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    val all = boundParams(All).isTruthy
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

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Unstage files")

}