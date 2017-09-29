package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.NoArgFunction
import com.github.mdr.mash.ns.core.NoArgFunction._
import com.github.mdr.mash.ns.git.{ GitCommon, GitHelper }
import com.github.mdr.mash.runtime.{ MashObject, MashString }
import org.eclipse.jgit.api.CreateBranchCommand.SetupUpstreamMode
import org.eclipse.jgit.api.ListBranchCommand.ListMode

import scala.collection.JavaConverters.asScalaBufferConverter

object CreateFunction extends MashFunction("git.branch.create") {

  object Params {
    lazy val Branch: Parameter = Parameter(
      nameOpt = Some("branch"),
      summaryOpt = Some("Name to give the new local branch"),
      defaultValueGeneratorOpt = Some(NoArgValue),
      descriptionOpt = Some(s"Can be omitted if '${Params.FromRemote.name}' is provided"))
    lazy val Switch = Parameter(
      nameOpt = Some("switch"),
      summaryOpt = Some("Switch to the new branch after creating it (default false)"),
      shortFlagOpt = Some('s'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    lazy val FromRemote = Parameter(
      nameOpt = Some("fromRemote"),
      summaryOpt = Some("Create the new branch as a local tracking branch of the given remote branch"),
      isFlag = true,
      defaultValueGeneratorOpt = Some(NoArgValue))
  }
  import Params._

  val params = ParameterModel(Branch, Switch, FromRemote)

  private def validateRemote(boundParams: BoundParams): Option[String] =
    NoArgFunction.option(boundParams(FromRemote)).map {
      case MashString(s, _) ⇒ s
      case obj @ MashObject(_, Some(RemoteBranchClass)) ⇒ RemoteBranchClass.Wrapper(obj).fullName.s
      case x ⇒ boundParams.throwInvalidArgument(FromRemote, "Must be a remote branch, but was " + x.typeName)
    }.map { s ⇒
      if (getRemoteBranches contains s)
        s
      else
        boundParams.throwInvalidArgument(FromRemote, s + "is not a valid remote branch")
    }

  def call(boundParams: BoundParams): MashObject = {
    val branchOpt = boundParams.validateStringOpt(Branch).map(_.s)
    val fromRemoteOpt = validateRemote(boundParams)
    val switch = boundParams(Switch).isTruthy
    if (branchOpt.isEmpty && fromRemoteOpt.isEmpty)
      throw EvaluatorException(s"Must provide at least one of '${Branch.name}' and '${FromRemote.name}'")

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
        branches.map(branch ⇒ GitCommon.trimRemoteBranchPrefix(branch.getName)).filterNot(_.endsWith("/HEAD"))
      }
    catch {
      case _: Exception ⇒ Seq()
    }

  override def typeInferenceStrategy = BranchClass

  override def summaryOpt = Some("Create a new local branch")

  override def descriptionOpt = Some(s"""If '${Params.FromRemote.name}' is provided, '${Params.Branch.name}' can be omitted, with the same name used locally.""")
}