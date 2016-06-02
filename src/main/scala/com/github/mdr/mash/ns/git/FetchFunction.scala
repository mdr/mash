package com.github.mdr.mash.ns.git

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import org.eclipse.jgit.transport.TrackingRefUpdate
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.git.branch.RemoteBranchNameClass
import com.github.mdr.mash.evaluator.MashList

object FetchFunction extends MashFunction("git.fetch") {

  val params = ParameterModel(Seq())

  def apply(arguments: Arguments): MashList = {
    params.validate(arguments)
    GitHelper.withGit { git ⇒
      val fetchResult = git.fetch.call()
      val updates = fetchResult.getTrackingRefUpdates.asScala.toSeq
      MashList(updates.map(asMashObject))
    }
  }

  private def asMashObject(update: TrackingRefUpdate): MashObject = {
    val branchName = update.getLocalName.replaceAll("^refs/remotes/", "")
    import FetchBranchUpdateClass.Fields._
    MashObject(
      ListMap(
        RemoteBranch -> MashString(branchName, RemoteBranchNameClass),
        OldCommit -> MashString(update.getOldObjectId.getName, CommitHashClass),
        NewCommit -> MashString(update.getNewObjectId.getName, CommitHashClass)),
      FetchBranchUpdateClass)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Seq(FetchBranchUpdateClass))

  override def summary = "Download objects and refs from another repository."

}