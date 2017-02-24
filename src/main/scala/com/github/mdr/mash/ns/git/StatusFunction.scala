package com.github.mdr.mash.ns.git

import java.{ util ⇒ ju }

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.git.branch.{ LocalBranchNameClass, RemoteBranchNameClass }
import com.github.mdr.mash.runtime._
import org.eclipse.jgit.api.{ Git, Status }
import org.eclipse.jgit.lib.BranchTrackingStatus

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

object StatusFunction extends MashFunction("git.status") {

  val params = ParameterModel()

  def apply(boundParams: BoundParams): MashObject = {
    GitHelper.withRepository { repo ⇒
      val branch = repo.getBranch
      val git = new Git(repo)
      val status = git.status.call()
      val branchTrackingStatusOpt = Option(BranchTrackingStatus.of(repo, branch))
      asMashObject(branch, status, branchTrackingStatusOpt)
    }
  }

  private def mashify(paths: ju.Set[String]): MashList = MashList(paths.asScala.toSeq.map(asPathString))

  private def trimUnwantedPrefix(remoteBranch: String): String =
    remoteBranch.replaceAll("^refs/remotes/", "")

  private def asMashObject(branch: String, status: Status, branchTrackingStatusOpt: Option[BranchTrackingStatus]): MashObject = {
    val modified = mashify(status.getModified)
    val untracked = mashify(status.getUntracked)
    val added = mashify(status.getAdded)
    val changed = mashify(status.getChanged)
    val removed = mashify(status.getRemoved)
    val missing = mashify(status.getMissing)
    val conflicting = mashify(status.getConflicting)
    val (upstreamBranch, aheadCount, behindCount) = mashify(branchTrackingStatusOpt)
    import StatusClass.Fields._
    MashObject.of(ListMap(
      Branch -> MashString(branch, LocalBranchNameClass),
      UpstreamBranch -> upstreamBranch,
      AheadCount -> aheadCount,
      BehindCount -> behindCount,
      Added -> added,
      Changed -> changed,
      Missing -> missing,
      Modified -> modified,
      Removed -> removed,
      Untracked -> untracked,
      Conflicting -> conflicting), StatusClass)

  }

  def mashify(statusOpt: Option[BranchTrackingStatus]) = statusOpt match {
    case Some(status) ⇒
      (
        MashString(status.getRemoteTrackingBranch.replaceAll("^refs/remotes/", ""), RemoteBranchNameClass),
        MashNumber(status.getAheadCount),
        MashNumber(status.getBehindCount))
    case None ⇒
      (MashNull, MashNumber(0), MashNumber(0))
  }

  override def typeInferenceStrategy = StatusClass

  override def summaryOpt = Some("Return the working tree status")

}
