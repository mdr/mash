package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.git.GitHelper

object DeleteFunction extends MashFunction("git.branch.delete") {

  object Params {
    val Branches = Parameter(
      name = "branches",
      summary = "Local branches to delete",
      isVariadic = true,
      variadicAtLeastOne = true)
  }
  import Params._

  val params = ParameterModel(Seq(Branches))

  private def validateBranch(boundParams: BoundParams, branch: Any): String =
   branch match {
      case MashString(s, _)                       ⇒ s
      case obj @ MashObject(_, Some(LocalBranchClass)) ⇒ obj.field(LocalBranchClass.Fields.Name).asInstanceOf[MashString].s
      case _                                      ⇒ boundParams.throwInvalidArgument(Branches, "Must be a branch")
    }

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val branches = boundParams.validateSequence(Branches).map(branch ⇒ validateBranch(boundParams, branch))
    val localBranches = SwitchFunction.getLocalBranches.toSet
    for (branch ← branches)
      if (!localBranches.contains(branch))
        boundParams.throwInvalidArgument(Branches, s"'$branch' is not a local branch")
    GitHelper.withGit { git ⇒
      git.branchDelete.setForce(true).setBranchNames(branches: _*).call()
    }
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Branches ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches)
    }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Delete a local branch"
}