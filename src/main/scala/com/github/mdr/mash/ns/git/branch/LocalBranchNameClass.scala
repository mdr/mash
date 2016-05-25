package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.MashString
import org.eclipse.jgit.api.Git
import scala.collection.JavaConverters._
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.ns.git.MemberLifter

object LocalBranchNameClass extends MashClass("git.branch.LocalBranchName") {

  def summary = "A git local branch name"

  val lifter = new MemberLifter(getBranchInfo)

  override lazy val methods = Seq(
    lifter.liftField(BranchClass.Fields.Commit),
    lifter.liftField(BranchClass.Fields.UpstreamBranch),
    lifter.liftMethod(BranchClass.DeleteMethod),
    lifter.liftMethod(BranchClass.LogMethod),
    lifter.liftMethod(BranchClass.SwitchMethod),
    lifter.liftMethod(BranchClass.PushMethod),
    InfoMethod)

  object InfoMethod extends MashMethod("info") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashObject = {
      params.validate(arguments)
      val branchName = target.asInstanceOf[MashString]
      getBranchInfo(branchName)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(BranchClass))

    override def summary = "Get information about the local branch with this name"

  }

  private def getBranchInfo(branchName: MashString): MashObject =
    GitHelper.withRepository { repo ⇒
      val git = new Git(repo)
      val ref = git.branchList.call().asScala.find(_.getName == "refs/heads/" + branchName).getOrElse(
        throw new EvaluatorException("No branch with name " + branchName))
      ListFunction.asMashObject(repo)(ref)
    }

}
  
