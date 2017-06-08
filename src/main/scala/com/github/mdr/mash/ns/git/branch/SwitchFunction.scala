package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.runtime.MashUnit

import scala.collection.JavaConverters._

object SwitchFunction extends MashFunction("git.branch.switch") {

  override def aliases = Seq(FullyQualifiedName("git.switch"))

  object Params {
    val Branch = Parameter(
      nameOpt = Some("branch"),
      summaryOpt = Some("Name of a local branch to switch to"))
  }
  import Params._

  val params = ParameterModel(Branch)

  def call(boundParams: BoundParams): MashUnit = {
    val branch = DeleteFunction.validateBranch(boundParams, Branch, boundParams(Branch))
    GitHelper.withGit { git ⇒
      git.checkout().setName(branch).call()
    }
    MashUnit
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

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Switch to a local branch")

}