package com.github.mdr.mash.ns.git.branch

import scala.collection.JavaConverters._
import org.eclipse.jgit.lib.Ref
import org.eclipse.jgit.lib.Repository
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.git.branch.BranchClass.Fields
import com.github.mdr.mash.ns.git.CommitHashClass
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.ns.git.StatusFunction
import org.eclipse.jgit.lib.BranchTrackingStatus
import org.eclipse.jgit.api.Git
import scala.collection.immutable.ListMap
import org.eclipse.jgit.api.ListBranchCommand.ListMode
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashList

object ListRemoteFunction extends MashFunction("git.branch.remoteBranches") {

  val params = ParameterModel()

  def apply(arguments: Arguments): MashList = {
    params.validate(arguments)
    GitHelper.withRepository { repo ⇒
      val git = new Git(repo)
      val branches = git.branchList.setListMode(ListMode.REMOTE).call().asScala.filterNot(_.getName endsWith "/HEAD")
      MashList(branches.map(asMashObject(repo)))
    }
  }

  def asMashObject(repo: Repository)(ref: Ref): MashObject = {
    val id = ref.getObjectId.getName
    val name = ref.getName.replaceAll("^refs/remotes/", "")
    val Seq(remote, branchName) = name.split("/", 2).toSeq
    import RemoteBranchClass.Fields._
    MashObject.of(
      ListMap(
        Remote -> MashString(remote, RemoteNameClass),
        Name -> MashString(branchName, RemoteBranchNameClass),
        Commit -> MashString(id, CommitHashClass)),
      RemoteBranchClass)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Seq(RemoteBranchClass))

  override def summary = "List remote branches in the repository"

}