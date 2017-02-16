package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator.{ Arguments, EvaluatorException }
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.ns.git.{ GitHelper, MemberLifter }
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashValue }
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.ListBranchCommand.ListMode

import scala.collection.JavaConverters._

object RemoteBranchNameClass extends MashClass("git.branch.RemoteBranchName") {

  val lifter = new MemberLifter(getBranchInfo)

  override lazy val methods = Seq(
    lifter.liftField(RemoteBranchClass.Fields.Remote),
    lifter.liftField(RemoteBranchClass.Fields.Name),
    lifter.liftField(RemoteBranchClass.Fields.Commit),
    lifter.liftMethod(RemoteBranchClass.CreateLocalMethod),
    lifter.liftMethod(RemoteBranchClass.IsAncestorOfMethod),
    lifter.liftMethod(RemoteBranchClass.DeleteMethod),
    lifter.liftMethod(RemoteBranchClass.LogMethod),
    InfoMethod)

  private def getBranchInfo(branchName: MashString): MashObject =
    GitHelper.withRepository { repo ⇒
      val git = new Git(repo)
      val remoteBranches = git.branchList.setListMode(ListMode.REMOTE).call().asScala
      val ref = remoteBranches.find(_.getName == "refs/remotes/" + branchName).getOrElse(
        throw new EvaluatorException("No branch with name " + branchName))
      ListRemoteFunction.asMashObject(repo)(ref)
    }

  object InfoMethod extends MashMethod("info") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      params.validate(arguments)
      val branchName = target.asInstanceOf[MashString]
      getBranchInfo(branchName)
    }

    override def typeInferenceStrategy = BranchClass

    override def summaryOpt = Some("Get information about the local branch with this name")

  }

  override def summaryOpt = Some("A git remote branch name")

  override def parentOpt = Some(AnyClass)

}
