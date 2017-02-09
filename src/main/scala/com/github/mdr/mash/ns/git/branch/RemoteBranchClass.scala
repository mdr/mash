package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.classes.{ Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.git._
import com.github.mdr.mash.ns.git.remote.RemoteNameClass
import com.github.mdr.mash.runtime._
import org.eclipse.jgit.api.CreateBranchCommand.SetupUpstreamMode
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.transport.RefSpec

import scala.collection.JavaConverters._

object RemoteBranchClass extends MashClass("git.branch.RemoteBranch") {

  object Fields {
    val Remote = Field("remote", Some("Name of the remote this branch is in"), StringClass taggedWith RemoteNameClass)
    val Name = Field("name", Some("Name of the remote this branch is in"), StringClass)
    val Commit = Field("commit", Some("The commit the branch is pointing to"), StringClass taggedWith CommitHashClass)
  }

  import Fields._

  override lazy val fields = Seq(Remote, Name, Commit)

  override def summaryOpt = Some("A remote git branch")

  override lazy val methods = Seq(
    CreateLocalMethod,
    DeleteMethod,
    FullNameMethod,
    IsAncestorOfMethod,
    LogMethod,
    ToStringMethod)

  override val staticMethods = Seq(NewStaticMethod(this))

  case class Wrapper(target: MashValue) {

    def name = target.asInstanceOf[MashObject](Fields.Name).asInstanceOf[MashString]

    def remote = target.asInstanceOf[MashObject](Fields.Remote).asInstanceOf[MashString]

    def fullName: MashString = MashString(s"$remote/$name", RemoteBranchNameClass)

  }

  object CreateLocalMethod extends MashMethod("createLocal") {

    object Params {
      lazy val Switch = Parameter(
        nameOpt = Some("switch"),
        summaryOpt = Some("Switch to the new branch after creating it (default false)"),
        shortFlagOpt = Some('s'),
        isFlag = true,
        defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
        isBooleanFlag = true)
    }

    import Params._

    val params = ParameterModel(Seq(Switch))

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      val boundParams = params.validate(arguments)
      val switch = boundParams(Switch).isTruthy
      GitHelper.withGit { git ⇒
        val wrapper = Wrapper(target)
        val localName = wrapper.name.s
        val cmd = git.branchCreate.setName(localName)
        cmd.setStartPoint(wrapper.fullName.s)
        cmd.setUpstreamMode(SetupUpstreamMode.TRACK)
        val branchRef = cmd.call()
        val branch = ListFunction.asMashObject(git.getRepository)(branchRef)
        if (switch)
          git.checkout().setName(localName).call()
        branch
      }
    }

    override def typeInferenceStrategy = BranchClass

    override def summaryOpt = Some("Create a local branch tracking this remote branch")
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

    override def summaryOpt = Some("Delete this remote branch")

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

    override def typeInferenceStrategy = Seq(CommitClass)

    override def summaryOpt = Some("Return a list of commits from this branch")

  }

  object ToStringMethod extends AbstractToStringMethod {

    override def toString(target: MashValue) = Wrapper(target).fullName.s

  }

  object FullNameMethod extends MashMethod("fullName") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      Wrapper(target).fullName
    }

    override def typeInferenceStrategy = StringClass

    override def summaryOpt = Some("Full name of the remote branch, e.g. origin/master")

  }

}