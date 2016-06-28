package com.github.mdr.mash.ns.git.branch

import scala.collection.JavaConverters._
import org.eclipse.jgit.api.Git
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.CommitClass
import com.github.mdr.mash.ns.git.CommitHashClass
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.ns.git.LogFunction
import com.github.mdr.mash.ns.git.PushFunction
import com.github.mdr.mash.ns.core.StringClass
import org.eclipse.jgit.transport.RefSpec
import com.github.mdr.mash.ns.git.AbstractIsAncestorOfMethod
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashUnit
import com.github.mdr.mash.runtime.MashValue

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
    DeleteMethod,
    FullNameMethod,
    IsAncestorOfMethod,
    LogMethod,
    ToStringMethod)

  case class Wrapper(target: MashValue) {

    def name = target.asInstanceOf[MashObject].field(Fields.Name).asInstanceOf[MashString]

    def remote = target.asInstanceOf[MashObject].field(Fields.Remote).asInstanceOf[MashString]

    def fullName: MashString = MashString(s"$remote/$name", RemoteBranchNameClass)

  }

  object DeleteMethod extends MashMethod("delete") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashUnit = {
      params.validate(arguments)
      val wrapper = Wrapper(target)
      GitHelper.withGit { git ⇒
        val refSpec = new RefSpec(":" + wrapper.name)
        git.push.setRemote(wrapper.remote.s).setRefSpecs(refSpec).call()
      }
      MashUnit
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summary = "Delete this remote branch"

  }

  object IsAncestorOfMethod extends AbstractIsAncestorOfMethod {

    override def commitName(target: MashValue) = Wrapper(target).fullName.s

  }

  object LogMethod extends MashMethod("log") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
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

    val params = AnyClass.ToStringMethod.params

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      Wrapper(target).fullName
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

    override def summary = AnyClass.ToStringMethod.summary

  }

  object FullNameMethod extends MashMethod("fullName") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      Wrapper(target).fullName
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

    override def summary = "Full name of the remote branch, e.g. origin/master"

  }

}