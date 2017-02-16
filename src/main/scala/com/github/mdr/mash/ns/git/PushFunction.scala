package com.github.mdr.mash.ns.git

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantTypeInferenceStrategy, TypedArguments }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.branch.{ DeleteFunction, SwitchFunction }
import com.github.mdr.mash.runtime.{ MashBoolean, MashNull, MashUnit }
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.{ ConfigConstants, Constants }

import scala.collection.JavaConverters._

object PushFunction extends MashFunction("git.push") {

  object Params {
    val SetUpstream = Parameter(
      nameOpt = Some("setUpstream"),
      summaryOpt = Some("Add upstream tracking branch (default false)"),
      shortFlagOpt = Some('u'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
    val Force = Parameter(
      nameOpt = Some("force"),
      summaryOpt = Some("Force push (default false)"),
      shortFlagOpt = Some('f'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
    val Remote = Parameter(
      nameOpt = Some("remote"),
      isFlag = true,
      isFlagValueMandatory = true,
      summaryOpt = Some("Remote to push to"),
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))
    val Branches = Parameter(
      nameOpt = Some("branches"),
      summaryOpt = Some("Local branch to push"),
      isVariadic = true)
  }
  import Params._

  val params = ParameterModel(Seq(SetUpstream, Force, Remote, Branches))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val branches = DeleteFunction.validateBranches(boundParams, Branches)
    val remoteOpt = boundParams.validateStringOpt(Remote).map(_.s)

    val setUpstream = boundParams(SetUpstream).isTruthy
    val force = boundParams(Force).isTruthy

    GitHelper.withGit { git ⇒
      val cmd = git.push
      for (branch ← branches)
        cmd.add(branch)
      for (remote ← remoteOpt)
        cmd.setRemote(remote)
      cmd.setForce(force)
      cmd.call()
      if (setUpstream)
        setUpstreamConfig(git, branches, remoteOpt)
    }
    MashUnit
  }

  def setUpstreamConfig(git: Git, refs: Seq[String], remoteOpt: Option[String]) {
    val config = git.getRepository.getConfig
    for (ref ← refs) {
      val remoteName = remoteOpt.getOrElse(Constants.DEFAULT_REMOTE_NAME)
      config.setString(ConfigConstants.CONFIG_BRANCH_SECTION, ref, ConfigConstants.CONFIG_KEY_REMOTE, remoteName)
      config.setString(ConfigConstants.CONFIG_BRANCH_SECTION, ref, ConfigConstants.CONFIG_KEY_MERGE, "refs/heads/" + ref)
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