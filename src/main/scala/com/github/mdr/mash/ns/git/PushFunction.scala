package com.github.mdr.mash.ns.git

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.NoArgFunction._
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.branch.{ DeleteFunction, SwitchFunction }
import com.github.mdr.mash.runtime._
import org.eclipse.jgit.api.errors.TransportException
import org.eclipse.jgit.api.{ Git, PushCommand }
import org.eclipse.jgit.lib.{ ConfigConstants, Constants }
import org.eclipse.jgit.transport.RemoteRefUpdate

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

object PushFunction extends MashFunction("git.push") {

  object Params {
    val SetUpstream = Parameter(
      nameOpt = Some("setUpstream"),
      summaryOpt = Some("Add upstream tracking branch (default false)"),
      shortFlagOpt = Some('u'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isBooleanFlag = true)
    val Force = Parameter(
      nameOpt = Some("force"),
      summaryOpt = Some("Force push (default false)"),
      shortFlagOpt = Some('f'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isBooleanFlag = true)
    val Remote = Parameter(
      nameOpt = Some("remote"),
      isFlag = true,
      isFlagValueMandatory = true,
      summaryOpt = Some("Remote to push to"),
      defaultValueGeneratorOpt = Some(NoArgValue))
    val Branches = Parameter(
      nameOpt = Some("branches"),
      summaryOpt = Some("Local branch to push"),
      isVariadic = true)
  }

  import Params._

  val params = ParameterModel(SetUpstream, Force, Remote, Branches)

  def call(boundParams: BoundParams): MashList = {
    val branches = DeleteFunction.validateBranches(boundParams, Branches)
    val remoteOpt = boundParams.validateStringOpt(Remote).map(_.s)
    val setUpstream = boundParams(SetUpstream).isTruthy
    val force = boundParams(Force).isTruthy

    GitHelper.withGit { git ⇒
      val cmd = makeCommand(git, branches, remoteOpt, force)

      val pushResults =
        try cmd.call().asScala.toSeq
        catch {
          case e: TransportException ⇒ throw new EvaluatorException(s"Error pushing: ${e.getMessage}")
        }
      val results =
        for {
          pushResult ← pushResults
          remoteUpdate ← pushResult.getRemoteUpdates.asScala
        } yield makeResultObject(remoteUpdate)

      if (setUpstream)
        setUpstreamConfig(git, branches, remoteOpt)

      MashList(results)
    }
  }

  private def makeCommand(git: Git, branches: Seq[String], remoteOpt: Option[String], force: Boolean): PushCommand = {
    val cmd = git.push
    for (branch ← branches)
      cmd.add(branch)
    for (remote ← remoteOpt)
      cmd.setRemote(remote)
    cmd.setForce(force)
    cmd
  }

  private def makeResultObject(remoteUpdate: RemoteRefUpdate): MashObject =
    MashObject.of(ListMap(
      "remote" -> MashString(remoteUpdate.getRemoteName),
      "status" -> MashString(remoteUpdate.getStatus.toString),
      "message" -> Option(remoteUpdate.getMessage).map(MashString(_)).getOrElse(MashNull),
      "isFastForward" -> MashBoolean(remoteUpdate.isFastForward),
      "isForceUpdate" -> MashBoolean(remoteUpdate.isForceUpdate)))

  def setUpstreamConfig(git: Git, branches: Seq[String], remoteOpt: Option[String]) {
    val repo = git.getRepository
    val config = repo.getConfig
    val actualBranches = if (branches.isEmpty) Seq(repo.getFullBranch.replaceAll("^refs/heads/", "")) else branches
    for (branch ← actualBranches) {
      val remoteName = remoteOpt.getOrElse(Constants.DEFAULT_REMOTE_NAME)
      config.setString(ConfigConstants.CONFIG_BRANCH_SECTION, branch, ConfigConstants.CONFIG_KEY_REMOTE, remoteName)
      config.setString(ConfigConstants.CONFIG_BRANCH_SECTION, branch, ConfigConstants.CONFIG_KEY_MERGE, "refs/heads/" + branch)
    }
    config.save()
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Update remote refs along with associated objects")

  def getRemotes: Seq[String] =
    GitHelper.withRepository { repo ⇒
      repo.getConfig.getSubsections("remote").asScala.toSeq
    }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Remote   ⇒ CompletionSpec.Items(getRemotes)
      case Branches ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches)
    }
}