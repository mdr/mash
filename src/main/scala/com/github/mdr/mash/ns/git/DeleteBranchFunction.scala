package com.github.mdr.mash.ns.git

import scala.collection.JavaConverters._
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.evaluator.EvaluatorException
import org.eclipse.jgit.api.CreateBranchCommand.SetupUpstreamMode
import org.eclipse.jgit.api.ListBranchCommand.ListMode

object DeleteBranchFunction extends MashFunction("git.deleteBranch") {

  object Params {
    lazy val Branch: Parameter = Parameter(
      name = "branch",
      summary = "Name to give the new local branch")
  }
  import Params._

  val params = ParameterModel(Seq(Branch))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val branch = boundParams.validateString(Branch).s
    GitHelper.withGit { git ⇒
      git.branchDelete().setBranchNames(branch)
    }
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Branch ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches)
    }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def summary = "Delete a local branch"
}