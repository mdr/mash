package com.github.mdr.mash.ns.git.branch

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
import com.github.mdr.mash.functions.FullyQualifiedName
import com.github.mdr.mash.ns.git.GitHelper

object SwitchFunction extends MashFunction("git.branch.switch") {

  override def aliases = Seq(FullyQualifiedName("git.switch"))
  
  object Params {
    val Branch = Parameter(
      name = "branch",
      summary = "Name of a local branch to switch to")
  }
  import Params._

  val params = ParameterModel(Seq(Branch))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val branch = boundParams.validateString(Branch).s
    GitHelper.withGit { git ⇒
      git.checkout().setName(branch).call()
    }
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    Seq(CompletionSpec.Items(getLocalBranches))

  def getLocalBranches: Seq[String] =
    try
      GitHelper.withGit { git ⇒
        val branches = git.branchList.call().asScala
        branches.map(_.getName.replaceAll("^refs/heads/", ""))
      }
    catch {
      case _: Exception ⇒ Seq()
    }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def summary = "Switch to a local branch"

}