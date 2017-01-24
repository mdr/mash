package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.{ Field, MashClass }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.git.branch.RemoteBranchNameClass

object FetchBranchUpdateClass extends MashClass("git.FetchBranchUpdate") {

  object Fields {
    val RemoteBranch = Field("branch", Some("Remote branch"), StringClass taggedWith RemoteBranchNameClass)
    val OldCommit = Field("oldCommit", Some("Old commit prior to the fetch"), StringClass taggedWith CommitHashClass)
    val NewCommit = Field("newCommit", Some("New commit prior to the fetch"), StringClass taggedWith CommitHashClass)
  }
  import Fields._

  override lazy val fields = Seq(RemoteBranch, OldCommit, NewCommit)

  override lazy val methods = Seq()

  def summary = "A branch update after a fetch"

}