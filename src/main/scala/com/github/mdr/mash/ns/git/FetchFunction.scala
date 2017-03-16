package com.github.mdr.mash.ns.git

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.git.branch.RemoteBranchNameClass
import com.github.mdr.mash.runtime._
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.transport.TrackingRefUpdate

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

object FetchFunction extends MashFunction("git.fetch") {

  val params = ParameterModel(Seq())

  override def apply(boundParams: BoundParams): MashList = {
    GitHelper.withGit { git ⇒
      val fetchResult = git.fetch.call()
      val updates = fetchResult.getTrackingRefUpdates.asScala.toSeq
      MashList(updates.map(asMashObject))
    }
  }

  def asMashObject(update: TrackingRefUpdate): MashObject = {
    val branchName = update.getLocalName.replaceAll("^refs/remotes/", "")
    import FetchBranchUpdateClass.Fields._
    MashObject.of(
      ListMap(
        RemoteBranch -> MashString(branchName, RemoteBranchNameClass),
        OldCommit -> asCommitHash(update.getOldObjectId),
        NewCommit -> asCommitHash(update.getNewObjectId)),
      FetchBranchUpdateClass)
  }

  private def asCommitHash(id: ObjectId): MashValue =
    if (id == ObjectId.zeroId) MashNull else MashString(id.name, CommitHashClass)

  override def typeInferenceStrategy = Seq(FetchBranchUpdateClass)

  override def summaryOpt = Some("Download objects and refs from another repository.")

}