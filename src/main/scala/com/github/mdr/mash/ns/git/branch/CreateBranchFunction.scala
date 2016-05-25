package com.github.mdr.mash.ns.git.branch

import scala.collection.JavaConverters._
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.evaluator.EvaluatorException
import org.eclipse.jgit.api.CreateBranchCommand.SetupUpstreamMode
import org.eclipse.jgit.api.ListBranchCommand.ListMode
import com.github.mdr.mash.ns.git.GitHelper

object CreateBranchFunction extends MashFunction("git.branch.create") {

  object Params {
    lazy val Branch: Parameter = Parameter(
      name = "branch",
      summary = "Name to give the new local branch",
      defaultValueGeneratorOpt = Some(() ⇒ null),
      descriptionOpt = Some(s"Can be omitted if '${Params.FromRemote.name}' is provided"))
    lazy val Switch = Parameter(
      name = "switch",
      summary = "Switch to the new branch after creating it (default false)",
      shortFlagOpt = Some('s'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isBooleanFlag = true)
    lazy val FromRemote = Parameter(
      name = "fromRemote",
      summary = "Create the new branch as a local tracking branch of the given remote branch",
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ null))
  }
  import Params._

  val params = ParameterModel(Seq(Branch, Switch, FromRemote))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val branchOpt = boundParams.validateStringOpt(Branch).map(_.s)
    val fromRemoteOpt = boundParams.validateStringOpt(FromRemote).map(_.s)
    val switch = Truthiness.isTruthy(boundParams(Switch))
    if (branchOpt.isEmpty && fromRemoteOpt.isEmpty)
      throw new EvaluatorException(s"Must provide at least one of '${Branch.name}' and '${FromRemote.name}'")
    GitHelper.withGit { git ⇒
      val localName = branchOpt.orElse(fromRemoteOpt.map(_.replaceAll("^origin/", ""))).get
      val cmd = git.branchCreate().setName(localName)
      for (remoteName ← fromRemoteOpt) {
        cmd.setStartPoint(remoteName)
        cmd.setUpstreamMode(SetupUpstreamMode.TRACK)
      }
      cmd.call()
      if (switch)
        git.checkout().setName(localName).call()
    }
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case FromRemote ⇒ CompletionSpec.Items(getRemoteBranches)
    }

  private def getRemoteBranches: Seq[String] =
    try
      GitHelper.withGit { git ⇒
        val branches = git.branchList.setListMode(ListMode.REMOTE).call().asScala
        branches.map(_.getName.replaceAll("^refs/remotes/", ""))
      }
    catch {
      case _: Exception ⇒ Seq()
    }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def summary = "Create a new local branch"

  override def descriptionOpt = Some(s"""If '${Params.FromRemote.name}' is provided, '${Params.Branch.name}' can be omitted, with the same name used locally.""")
}