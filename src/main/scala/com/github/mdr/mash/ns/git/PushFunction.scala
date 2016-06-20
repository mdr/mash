package com.github.mdr.mash.ns.git

import scala.collection.JavaConverters._
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.ConfigConstants
import org.eclipse.jgit.lib.Constants
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.git.branch.SwitchFunction
import com.github.mdr.mash.ns.git.branch.DeleteFunction
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.runtime.MashUnit

object PushFunction extends MashFunction("git.push") {

  object Params {
    val SetUpstream = Parameter(
      name = "setUpstream",
      summary = "Add upstream tracking branch (default false)",
      shortFlagOpt = Some('u'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
    val Force = Parameter(
      name = "force",
      summary = "Force push (default false)",
      shortFlagOpt = Some('f'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
    val Remote = Parameter(
      name = "remote",
      isFlag = true,
      summary = "Remote to push to",
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))
    val Branches = Parameter(
      name = "branches",
      summary = "Local branch to push",
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
      config.setString(ConfigConstants.CONFIG_BRANCH_SECTION, ref, "remote", remoteName)
      config.setString(ConfigConstants.CONFIG_BRANCH_SECTION, ref, "merge", "refs/heads/" + ref)
    }
    config.save()
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Update remote refs along with associated objects"

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