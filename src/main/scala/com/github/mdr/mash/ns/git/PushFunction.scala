package com.github.mdr.mash.ns.git

import org.eclipse.jgit.api.Git
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.evaluator.MashList
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.ns.core.UnitClass
import org.eclipse.jgit.lib.ConfigConstants
import org.eclipse.jgit.lib.Constants

object PushFunction extends MashFunction("git.push") {

  object Params {
    val SetUpstream = Parameter(
      name = "setUpstream",
      summary = "Add upstream tracking branch (default false)",
      shortFlagOpt = Some('u'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isBooleanFlag = true)
    val Force = Parameter(
      name = "force",
      summary = "Force push (default false)",
      shortFlagOpt = Some('f'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isBooleanFlag = true)
    val Remote = Parameter(
      name = "remote",
      summary = "Remote to push to",
      defaultValueGeneratorOpt = Some(() ⇒ null))
    val References = Parameter(
      name = "refs",
      summary = "References to push",
      isVariadic = true)
  }
  import Params._

  val params = ParameterModel(Seq(SetUpstream, Remote, References))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val refs = boundParams(References).asInstanceOf[MashList].items.map(ToStringifier.stringify)
    val remoteOpt = boundParams.validateStringOpt(Remote).map(_.s)
    val setUpstream = Truthiness.isFalsey(boundParams(SetUpstream))
    val force = Truthiness.isFalsey(boundParams(Force))

    GitHelper.withGit { git ⇒
      val cmd = git.push
      for (ref ← refs)
        cmd.add(ref)
      for (remote ← remoteOpt)
        cmd.setRemote(remote)
      cmd.setForce(force)
      cmd.call()
      if (setUpstream)
        setUpstreamConfig(git, refs, remoteOpt)
    }
  }

  private def setUpstreamConfig(git: Git, refs: Seq[String], remoteOpt: Option[String]) {
    val config = git.getRepository.getConfig
    for (ref ← refs) {
      val remoteName = remoteOpt.getOrElse(Constants.DEFAULT_REMOTE_NAME)
      config.setString(ConfigConstants.CONFIG_BRANCH_SECTION, ref, "remote", remoteName)
      config.setString(ConfigConstants.CONFIG_BRANCH_SECTION, ref, "merge", "refs/heads/" + ref)
    }
    config.save()
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def summary = "Update remote refs along with associated objects"

}