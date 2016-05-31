package com.github.mdr.mash.ns.git.branch

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import org.eclipse.jgit.api._
import org.eclipse.jgit.lib._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.git.CommitHashClass
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.ns.git.StatusFunction
import com.github.mdr.mash.ns.git.branch.LocalBranchClass.Fields
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy

object ListFunction extends MashFunction("git.branch.localBranches") {

  override def aliases = Seq(FullyQualifiedName("git.branches"))

  val params = ParameterModel()

  def apply(arguments: Arguments): MashList = {
    params.validate(arguments)
    GitHelper.withRepository { repo ⇒
      val git = new Git(repo)
      val branches = git.branchList.call().asScala.filter(_.getName startsWith "refs/heads/")
      MashList(branches.map(asMashObject(repo)))
    }
  }

  def asMashObject(repo: Repository)(ref: Ref): MashObject = {
    val id = ref.getObjectId.getName
    val (upstreamBranch, aheadCount, behindCount) =
      StatusFunction.mashify(Option(BranchTrackingStatus.of(repo, ref.getName)))
    val name = ref.getName.replaceAll("^refs/heads/", "")
    import LocalBranchClass.Fields._
    MashObject(
      ListMap(
        Name -> MashString(name, LocalBranchNameClass),
        Commit -> MashString(id, CommitHashClass),
        UpstreamBranch -> upstreamBranch,
        AheadCount -> aheadCount,
        BehindCount -> behindCount),
      LocalBranchClass)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Seq(LocalBranchClass))

  override def summary = "List branches in the repository"

}