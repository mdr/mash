package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.{ GitHelper, MergeFunction }
import com.github.mdr.mash.runtime.MashUnit
import org.eclipse.jgit.lib.ObjectId

object SetCommitFunction extends MashFunction("git.branch.setCommit") {

  object Params {
    val Branch = Parameter(
      nameOpt = Some("branch"),
      summaryOpt = Some("Local branch to update"))
    val Commit = Parameter(
      nameOpt = Some("commit"),
      summaryOpt = Some("Commit to point to"))
  }
  import Params._

  val params = ParameterModel(Branch, Commit)

  def call(boundParams: BoundParams): MashUnit = {
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

  override def typeInferenceStrategy = Unit

  override def summaryOpt = Some("Update a branch to point to a different commit")
}