package com.github.mdr.mash.ns.git

import org.eclipse.jgit.lib.ObjectId

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.BoundParams
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type.unitToType
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.git.branch.CreateFunction
import com.github.mdr.mash.ns.git.branch.LocalBranchClass
import com.github.mdr.mash.ns.git.branch.RemoteBranchClass
import com.github.mdr.mash.ns.git.branch.SwitchFunction
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashUnit

object MergeFunction extends MashFunction("git.merge") {

  object Params {
    val Commit = Parameter(
      name = "commit",
      summary = "Name of a commit to merge")
    val Squash = Parameter(
      name = "squash",
      summary = "Squash commits (default false)",
      shortFlagOpt = Some('s'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Commit, Squash))

  def validateCommit(boundParams: BoundParams, param: Parameter): ObjectId = {
    val commit = boundParams(param) match {
      case MashString(s, _) ⇒ s
      case obj @ MashObject(_, Some(LocalBranchClass)) ⇒ LocalBranchClass.Wrapper(obj).name.s
      case obj @ MashObject(_, Some(RemoteBranchClass)) ⇒ RemoteBranchClass.Wrapper(obj).fullName.s
      case obj @ MashObject(_, Some(CommitClass)) ⇒ CommitClass.Wrapper(obj).hash.s
      case _ ⇒ boundParams.throwInvalidArgument(param, "Must be a name of a commit")
    }
    GitHelper.withGit { git ⇒
      Option(git.getRepository.resolve(commit)).getOrElse(
        boundParams.throwInvalidArgument(Commit, "Must be the name of a commit"))
    }
  }

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
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

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Merge another commit into this branch"

}