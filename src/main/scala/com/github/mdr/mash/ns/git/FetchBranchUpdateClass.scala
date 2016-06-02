package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.git.branch.LocalBranchNameClass
import com.github.mdr.mash.ns.git.branch.RemoteBranchNameClass

object FetchBranchUpdateClass extends MashClass("git.FetchBranchUpdate") {

  object Fields {
    val RemoteBranch = Field("branch", "Remote branch", Type.Tagged(StringClass, RemoteBranchNameClass))
    val OldCommit = Field("oldCommit", "Old commit prior to the fetch", Type.Tagged(StringClass, CommitHashClass))
    val NewCommit = Field("newCommit", "New commit prior to the fetch", Type.Tagged(StringClass, CommitHashClass))
  }
  import Fields._

  override lazy val fields = Seq(RemoteBranch, OldCommit, NewCommit)

  override lazy val methods = Seq()

  def summary = "A branch update after a fetch"

}