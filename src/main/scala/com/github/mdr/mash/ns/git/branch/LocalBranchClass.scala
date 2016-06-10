package com.github.mdr.mash.ns.git.branch

import scala.collection.JavaConverters._
import org.eclipse.jgit.api.Git
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.CommitClass
import com.github.mdr.mash.ns.git.CommitHashClass
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.ns.git.LogFunction
import com.github.mdr.mash.ns.git.PushFunction
import com.github.mdr.mash.ns.git.MergeFunction
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.ns.core.BooleanClass
import org.eclipse.jgit.revwalk.RevWalk
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.ns.git.AbstractIsAncestorOfMethod
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashUnit
import com.github.mdr.mash.runtime.MashValue

object LocalBranchClass extends MashClass("git.branch.Branch") {

  object Fields {
    val Name = Field("name", "Name of the branch", Type.Tagged(StringClass, LocalBranchNameClass))
    val Commit = Field("commit", "The commit the branch is pointing to", Type.Tagged(StringClass, CommitHashClass))
    val UpstreamBranch = Field("upstreamBranch", "The upstream branch this branch is tracking, if any, else null", Type.Tagged(StringClass, RemoteBranchNameClass))
    val AheadCount = Field("aheadCount", "Number of commits that the local branch is ahead of the remote-tracking branch", NumberClass)
    val BehindCount = Field("behindCount", "Number of commits that the local branch is behind the remote-tracking branch", NumberClass)
  }

  import Fields._

  override lazy val fields = Seq(Name, Commit, UpstreamBranch, AheadCount, BehindCount)

  def summary = "A git branch"

  override lazy val methods = Seq(
    DeleteMethod,
    LogMethod,
    IsAncestorOfMethod,
    PushMethod,
    SetCommitMethod,
    SwitchMethod,
    ToStringMethod)

  case class Wrapper(target: MashValue) {

    def name = target.asInstanceOf[MashObject].field(Fields.Name).asInstanceOf[MashString]

  }

  object DeleteMethod extends MashMethod("delete") {

    import PushFunction.Params._

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashUnit = {
      params.validate(arguments)
      val branchName = Wrapper(target).name.s
      GitHelper.withGit { git ⇒
        git.branchDelete.setBranchNames(branchName).setForce(true).call()
      }
      MashUnit
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summary = "Delete this branch"

  }

  object IsAncestorOfMethod extends AbstractIsAncestorOfMethod {

    override def commitName(target: MashValue) = Wrapper(target).name.s

  }

  object LogMethod extends MashMethod("log") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      val branchName = Wrapper(target).name.s
      GitHelper.withRepository { repo ⇒
        val git = new Git(repo)
        val branchId = repo.resolve(branchName)
        val commits = git.log.add(branchId).call().asScala.toSeq.reverse
        MashList(commits.map(LogFunction.asCommitObject))
      }
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Seq(CommitClass))

    override def summary = "Return a list of commits from this branch"

  }

  object PushMethod extends MashMethod("push") {

    import PushFunction.Params._

    val params = ParameterModel(Seq(Force, SetUpstream, Remote))

    def apply(target: MashValue, arguments: Arguments): MashUnit = {
      val boundParams = params.validate(arguments)
      val force = Truthiness.isTruthy(boundParams(Force))
      val setUpstream = Truthiness.isTruthy(boundParams(SetUpstream))
      val remoteOpt = boundParams.validateStringOpt(Remote).map(_.s)
      val branchName = Wrapper(target).name.s
      GitHelper.withGit { git ⇒
        val cmd = git.push
        cmd.add(branchName).setForce(force)
        for (remote ← remoteOpt)
          cmd.setRemote(remote)
        cmd.call()
        if (setUpstream)
          PushFunction.setUpstreamConfig(git, Seq(branchName), remoteOpt)
      }
      MashUnit
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summary = "Push this branch"

  }

  object SwitchMethod extends MashMethod("switch") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashUnit = {
      params.validate(arguments)
      val branchName = Wrapper(target).name.s
      GitHelper.withGit { git ⇒
        git.checkout().setName(branchName).call()
      }
      MashUnit
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summary = "Switch to this branch"

  }

  object SetCommitMethod extends MashMethod("setCommit") {

    object Params {
      val Commit = SetCommitFunction.Params.Commit
    }
    import Params._

    val params = ParameterModel(Seq(Commit))

    def apply(target: MashValue, arguments: Arguments): MashUnit = {
      val boundParams = params.validate(arguments)
      val branch = Wrapper(target).name.s
      val commit = MergeFunction.validateCommit(boundParams, Commit)
      SetCommitFunction.setCommit(branch, commit)
      MashUnit
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summary = "Update this branch to point to a given commit"

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
      params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
        case Commit ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches ++ CreateFunction.getRemoteBranches)
      }
  }

  object ToStringMethod extends MashMethod("toString") {

    val params = ObjectClass.ToStringMethod.params

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      Wrapper(target).name
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

    override def summary = ObjectClass.ToStringMethod.summary

  }

}