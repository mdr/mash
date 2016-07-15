package com.github.mdr.mash.ns.git.branch

import scala.collection.JavaConverters.asScalaBufferConverter

import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.ListBranchCommand.ListMode

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.ns.git.MemberLifter
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashValue

object RemoteBranchNameClass extends MashClass("git.branch.RemoteBranchName") {

  val lifter = new MemberLifter(getBranchInfo)

  override lazy val methods = Seq(
    lifter.liftField(RemoteBranchClass.Fields.Remote),
    lifter.liftField(RemoteBranchClass.Fields.Name),
    lifter.liftField(RemoteBranchClass.Fields.Commit),
    lifter.liftMethod(RemoteBranchClass.IsAncestorOfMethod),
    MashClass.alias("isMergedInto", lifter.liftMethod(RemoteBranchClass.IsAncestorOfMethod)),
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

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BranchClass)

    override def summary = "Get information about the local branch with this name"

  }

  def summary = "A git remote branch name"

  override def parentOpt = Some(AnyClass)

}
