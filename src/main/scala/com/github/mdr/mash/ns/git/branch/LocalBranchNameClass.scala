package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.ns.git.{ GitHelper, MemberLifter }
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashValue }
import org.eclipse.jgit.api.Git

import scala.collection.JavaConverters._

object LocalBranchNameClass extends MashClass("git.branch.LocalBranchName") {

  override def summaryOpt = Some("A git local branch name")

  val lifter = new MemberLifter(getBranchInfo)

  override lazy val methods = Seq(
    lifter.liftField(BranchClass.Fields.Commit),
    lifter.liftField(BranchClass.Fields.UpstreamBranch),
    lifter.liftMethod(BranchClass.DeleteMethod),
    lifter.liftMethod(BranchClass.IsAncestorOfMethod),
    lifter.liftMethod(BranchClass.LogMethod),
    lifter.liftMethod(BranchClass.SetCommitMethod),
    lifter.liftMethod(BranchClass.SwitchMethod),
    lifter.liftMethod(BranchClass.PushMethod),
    InfoMethod)

  object InfoMethod extends MashMethod("info") {

    val params = ParameterModel()

    def call(target: MashValue, boundParams: BoundParams): MashObject = {
      val branchName = target.asInstanceOf[MashString]
      getBranchInfo(branchName)
    }

    override def typeInferenceStrategy = BranchClass

    override def summaryOpt = Some("Get information about the local branch with this name")

  }

  private def getBranchInfo(branchName: MashString): MashObject =
    GitHelper.withRepository { repo ⇒
      val git = new Git(repo)
      val ref = git.branchList.call().asScala.find(_.getName == "refs/heads/" + branchName).getOrElse(
        throw new EvaluatorException("No branch with name " + branchName))
      ListFunction.asMashObject(repo)(ref)
    }

  override def parentOpt = Some(AnyClass)

}
