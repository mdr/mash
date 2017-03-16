package com.github.mdr.mash.ns.git

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.branch.{ BranchClass, CreateFunction, RemoteBranchClass, SwitchFunction }
import com.github.mdr.mash.runtime.{ MashBoolean, MashObject, MashString, MashUnit }
import org.eclipse.jgit.lib.ObjectId

object MergeFunction extends MashFunction("git.merge") {

  object Params {
    val Commit = Parameter(
      nameOpt = Some("commit"),
      summaryOpt = Some("Name of a commit to merge"))
    val Squash = Parameter(
      nameOpt = Some("squash"),
      summaryOpt = Some("Squash commits (default false)"),
      shortFlagOpt = Some('s'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Commit, Squash))

  def validateCommit(boundParams: BoundParams, param: Parameter): ObjectId = {
    val commit = boundParams(param) match {
      case MashString(s, _) ⇒ s
      case obj @ MashObject(_, Some(BranchClass)) ⇒ BranchClass.Wrapper(obj).name
      case obj @ MashObject(_, Some(RemoteBranchClass)) ⇒ RemoteBranchClass.Wrapper(obj).fullName.s
      case obj @ MashObject(_, Some(CommitClass)) ⇒ CommitClass.Wrapper(obj).hash
      case _ ⇒ boundParams.throwInvalidArgument(param, "Must be a name of a commit")
    }
    GitHelper.withGit { git ⇒
      Option(git.getRepository.resolve(commit)).getOrElse(
        boundParams.throwInvalidArgument(Commit, "Must be the name of a commit"))
    }
  }

  def apply(boundParams: BoundParams): MashUnit = {
    val commit = validateCommit(boundParams, Commit)
    val squash = boundParams(Squash).isTruthy

    GitHelper.withGit { git ⇒
      git.merge.include(commit).setSquash(squash).call()
    }
    MashUnit
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Commit ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches ++ CreateFunction.getRemoteBranches)
    }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Merge another commit into this branch")

}