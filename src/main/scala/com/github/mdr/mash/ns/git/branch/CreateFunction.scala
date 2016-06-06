package com.github.mdr.mash.ns.git.branch

import scala.collection.JavaConverters.asScalaBufferConverter
import org.eclipse.jgit.api.CreateBranchCommand.SetupUpstreamMode
import org.eclipse.jgit.api.ListBranchCommand.ListMode
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.functions.BoundParams
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashObject

object CreateFunction extends MashFunction("git.branch.create") {

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

  private def validateRemote(boundParams: BoundParams): Option[String] =
    Option(boundParams(FromRemote)).map {
      case MashString(s, _) ⇒ s
      case obj @ MashObject(_, Some(RemoteBranchClass)) ⇒ RemoteBranchClass.Wrapper(obj).fullName.s
      case _ ⇒ boundParams.throwInvalidArgument(FromRemote, "Must be a remote branch")
    }.map { s ⇒
      if (getRemoteBranches contains s)
        s
      else
        boundParams.throwInvalidArgument(FromRemote, "Must be a remote branch")
    }

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val branchOpt = boundParams.validateStringOpt(Branch).map(_.s)
    val fromRemoteOpt = validateRemote(boundParams)
    val switch = Truthiness.isTruthy(boundParams(Switch))
    if (branchOpt.isEmpty && fromRemoteOpt.isEmpty)
      throw new EvaluatorException(s"Must provide at least one of '${Branch.name}' and '${FromRemote.name}'")

    GitHelper.withGit { git ⇒
      val localName = branchOpt.orElse(fromRemoteOpt.map(_.replaceAll("^origin/", ""))).get
      val cmd = git.branchCreate.setName(localName)
      for (remoteName ← fromRemoteOpt) {
        cmd.setStartPoint(remoteName)
        cmd.setUpstreamMode(SetupUpstreamMode.TRACK)
      }
      val branchRef = cmd.call()
      val branch = ListFunction.asMashObject(git.getRepository)(branchRef)
      if (switch)
        git.checkout().setName(localName).call()
      branch
    }
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case FromRemote ⇒ CompletionSpec.Items(getRemoteBranches)
    }

  def getRemoteBranches: Seq[String] =
    try
      GitHelper.withGit { git ⇒
        val branches = git.branchList.setListMode(ListMode.REMOTE).call().asScala
        branches.map(_.getName.replaceAll("^refs/remotes/", "")).filterNot(_.endsWith("/HEAD"))
      }
    catch {
      case _: Exception ⇒ Seq()
    }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(LocalBranchClass)

  override def summary = "Create a new local branch"

  override def descriptionOpt = Some(s"""If '${Params.FromRemote.name}' is provided, '${Params.Branch.name}' can be omitted, with the same name used locally.""")
}