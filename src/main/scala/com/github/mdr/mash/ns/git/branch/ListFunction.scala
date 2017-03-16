package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.git.{ CommitHashClass, GitHelper, StatusFunction }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString }
import org.eclipse.jgit.api._
import org.eclipse.jgit.lib._

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

object ListFunction extends MashFunction("git.branch.localBranches") {

  override def aliases = Seq(FullyQualifiedName("git.branches"))

  val params = ParameterModel()

  def apply(boundParams: BoundParams): MashList = {
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
    import BranchClass.Fields._
    MashObject.of(
      ListMap(
        Name -> MashString(name, LocalBranchNameClass),
        Commit -> MashString(id, CommitHashClass),
        UpstreamBranch -> upstreamBranch,
        AheadCount -> aheadCount,
        BehindCount -> behindCount),
      BranchClass)
  }

  override def typeInferenceStrategy = Seq(BranchClass)

  override def summaryOpt = Some("List branches in the repository")

}