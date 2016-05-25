package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.GitHelper

object DeleteBranchFunction extends MashFunction("git.branch.delete") {

  object Params {
    val Branch = Parameter(
      name = "branch",
      summary = "Name of the local branch to delete")
  }
  import Params._

  val params = ParameterModel(Seq(Branch))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val branch = boundParams.validateString(Branch).s
    GitHelper.withGit { git ⇒
      git.branchDelete().setForce(true).setBranchNames(branch).call()
    }
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Branch ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches)
    }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(UnitClass)

  override def summary = "Delete a local branch"
}