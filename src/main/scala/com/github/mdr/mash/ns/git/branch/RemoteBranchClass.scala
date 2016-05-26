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
import com.github.mdr.mash.ns.core.StringClass

object RemoteBranchClass extends MashClass("git.branch.RemoteBranch") {

  object Fields {
    val Remote = Field("remote", "Name of the remote this branch is in", Type.Tagged(StringClass, RemoteNameClass))
    val Name = Field("name", "Name of the remote this branch is in", StringClass)
    val Commit = Field("commit", "The commit the branch is pointing to", Type.Tagged(StringClass, CommitHashClass))
  }

  import Fields._

  override lazy val fields = Seq(Remote, Name, Commit)

  def summary = "A remote git branch"

  override lazy val methods = Seq(
    FullNameMethod,
    LogMethod,
    ToStringMethod)

  private case class Wrapper(target: Any) {

    def name = target.asInstanceOf[MashObject].field(Fields.Name).asInstanceOf[MashString]

    def remote = target.asInstanceOf[MashObject].field(Fields.Remote).asInstanceOf[MashString]

    def fullName: MashString = MashString(s"$remote/$name", RemoteBranchNameClass)

  }

  object LogMethod extends MashMethod("log") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashList = {
      params.validate(arguments)
      val branchName = Wrapper(target).fullName.s
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

  object ToStringMethod extends MashMethod("toString") {

    val params = ObjectClass.ToStringMethod.params

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      Wrapper(target).fullName
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

    override def summary = ObjectClass.ToStringMethod.summary

  }

  object FullNameMethod extends MashMethod("fullName") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      Wrapper(target).fullName
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

    override def summary = "Full name of the remote branch, e.g. origin/master"

  }

}