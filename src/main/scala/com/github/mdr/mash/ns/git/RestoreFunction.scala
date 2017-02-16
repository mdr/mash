package com.github.mdr.mash.ns.git

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.{ Arguments, EvaluatorException }
import com.github.mdr.mash.functions.{ FunctionHelpers, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashUnit }

import scala.collection.JavaConverters._
import scala.util.Try

object RestoreFunction extends MashFunction("git.restore") {

  object Params {
    val Paths = Parameter(
      nameOpt = Some("paths"),
      summaryOpt = Some("Restore the given paths"),
      isVariadic = true)
    val All = Parameter(
      nameOpt = Some("all"),
      summaryOpt = Some("Restore all modified paths (default false)"),
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
    val all = boundParams(All).isTruthy
    if (paths.isEmpty && !all)
      throw new EvaluatorException(s"Must provide either '$Paths' or '$All'")
    GitHelper.withGit { git ⇒
      val cmd = git.checkout.setStartPoint("HEAD")
      if (all)
        for (file ← getRestorableFiles)
          cmd.addPath(file)
      for (path ← paths)
        cmd.addPath(path.toString)
      cmd.call()
    }
    MashUnit
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Paths ⇒ Try(CompletionSpec.Items(getRestorableFiles)) getOrElse CompletionSpec.File
    }

  private def getRestorableFiles: Seq[String] = {
    val status = GitHelper.withGit { _.status.call() }
    (status.getModified.asScala ++ status.getMissing.asScala ++ status.getRemoved.asScala ++ status.getChanged.asScala).toSeq.distinct
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Restore modified or deleted files in the working directory")

}