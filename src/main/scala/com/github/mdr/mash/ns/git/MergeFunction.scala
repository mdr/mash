package com.github.mdr.mash.ns.git

import org.eclipse.jgit.api.Git
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.evaluator.MashList
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.branch.SwitchFunction
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.BoundParams
import com.github.mdr.mash.ns.git.branch.LocalBranchClass
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.ns.git.branch.RemoteBranchClass
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.ns.git.branch.CreateFunction

object MergeFunction extends MashFunction("git.merge") {

  object Params {
    val Commit = Parameter(
      name = "commit",
      summary = "Name of a commit to merge")
  }
  import Params._

  val params = ParameterModel(Seq(Commit))

  def validateCommit(boundParams: BoundParams, param: Parameter): String =
    boundParams(param) match {
      case MashString(s, _) ⇒ s
      case obj @ MashObject(_, Some(LocalBranchClass)) ⇒ LocalBranchClass.Wrapper(obj).name.s
      case obj @ MashObject(_, Some(RemoteBranchClass)) ⇒ RemoteBranchClass.Wrapper(obj).fullName.s
      case obj @ MashObject(_, Some(CommitClass)) ⇒ CommitClass.Wrapper(obj).hash.s
      case _ ⇒ boundParams.throwInvalidArgument(param, "Must be a name of a commit")
    }

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val commit = validateCommit(boundParams, Commit)
    GitHelper.withGit { git ⇒
      val objectId = Option(git.getRepository.resolve(commit)).getOrElse(
        boundParams.throwInvalidArgument(Commit, "Must be the name of a commit"))
      git.merge.include(objectId).call()
    }
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Commit ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches ++ CreateFunction.getRemoteBranches)
    }
  
  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Merge another commit into this branch"

}