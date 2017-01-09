package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantTypeInferenceStrategy, TypedArguments }
import com.github.mdr.mash.ns.git.{ GitHelper, MergeFunction }
import com.github.mdr.mash.runtime.MashUnit
import org.eclipse.jgit.lib.ObjectId

object SetCommitFunction extends MashFunction("git.branch.setCommit") {

  object Params {
    val Branch = Parameter(
      nameOpt = Some("branch"),
      summary = "Local branch to update")
    val Commit = Parameter(
      nameOpt = Some("commit"),
      summary = "Commit to point to")
  }
  import Params._

  val params = ParameterModel(Seq(Branch, Commit))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val branch = DeleteFunction.validateBranch(boundParams, Branch, boundParams(Branch))
    val commit = MergeFunction.validateCommit(boundParams, Commit)
    setCommit(branch, commit)
    MashUnit
  }

  def setCommit(branch: String, commit: ObjectId) {
    GitHelper.withRepository { repo ⇒
      val update = repo.updateRef("refs/heads/" + branch)
      update.setNewObjectId(commit)
      update.setForceUpdate(true)
      update.update()
    }
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Branch ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches)
      case Commit ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches ++ CreateFunction.getRemoteBranches)
    }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Update a branch to point to a different commit"
}