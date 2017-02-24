package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashUnit, MashValue }

object DeleteFunction extends MashFunction("git.branch.delete") {

  object Params {
    val Branches = Parameter(
      nameOpt = Some("branches"),
      summaryOpt = Some("Local branches to delete"),
      isVariadic = true,
      variadicAtLeastOne = true)
  }
  import Params._

  val params = ParameterModel(Seq(Branches))

  def validateBranch(boundParams: BoundParams, param: Parameter, branch: MashValue): String = {
    val branchName = branch match {
      case MashString(s, _) ⇒ s
      case obj @ MashObject(_, Some(BranchClass)) ⇒ BranchClass.Wrapper(obj).name
      case _ ⇒ boundParams.throwInvalidArgument(param, "Must be a local branch")
    }
    if (!SwitchFunction.getLocalBranches.contains(branchName))
      boundParams.throwInvalidArgument(param, "Must be a local branch")
    branchName
  }

  def validateBranches(boundParams: BoundParams, param: Parameter): Seq[String] =
    boundParams.validateSequence(param).map(branch ⇒ validateBranch(boundParams, param, branch))

  def apply(boundParams: BoundParams): MashUnit = {
    val branches = validateBranches(boundParams, Branches)
    GitHelper.withGit { git ⇒
      git.branchDelete.setForce(true).setBranchNames(branches: _*).call()
    }
    MashUnit
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Branches ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches)
    }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Delete a local branch")
}