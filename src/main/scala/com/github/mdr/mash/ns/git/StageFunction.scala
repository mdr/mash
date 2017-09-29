package com.github.mdr.mash.ns.git

import java.nio.file.Path

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashUnit }

import scala.collection.JavaConverters.asScalaSetConverter
import scala.util.Try

object StageFunction extends MashFunction("git.stage") {

  object Params {
    val All = Parameter(
      nameOpt = Some("all"),
      summaryOpt = Some("Stage all unstaged files (default false)"),
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Paths = Parameter(
      nameOpt = Some("paths"),
      summaryOpt = Some("Stage the given paths"),
      isVariadic = true,
      variadicFlatten = true)
  }

  import Params._

  val params = ParameterModel(All, Paths)

  def call(boundParams: BoundParams): MashUnit = {
    val paths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    val all = boundParams(All).isTruthy
    if (paths.isEmpty && !all)
      throw EvaluatorException(s"Must provide either '$Paths' or '$All'")
    if (paths.nonEmpty && all)
      boundParams.throwInvalidArgument(All, s"Cannot provide both '$Paths' and '$All'")

    doStage(paths, all)
    MashUnit
  }

  def doStage(paths: Seq[Path] = Seq(), all: Boolean = false) =
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

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Paths ⇒ Try(CompletionSpec.Items(getUnstagedFiles)) getOrElse CompletionSpec.File
    }

  private def getUnstagedFiles: Seq[String] = {
    val status = GitHelper.withGit {_.status.call()}
    (status.getUntracked.asScala ++
      status.getModified.asScala ++
      status.getMissing.asScala ++
      status.getConflicting.asScala).toSeq.distinct
  }

  override def typeInferenceStrategy = Unit

  override def summaryOpt = Some("Stage files")

}