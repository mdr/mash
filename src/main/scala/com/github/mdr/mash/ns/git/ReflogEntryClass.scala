package com.github.mdr.mash.ns.git

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.ns.core.StringClass

object ReflogEntryClass extends MashClass("git.ReflogEntry") {

  object Fields {
    val Commit = Field("commit", Some("Commit hash"), StringClass taggedWith CommitClass)
    val Comment = Field("comment", Some("Comment"), StringClass)
  }
  import Fields._

  override lazy val fields = Seq(Commit, Comment)

  override def summaryOpt: Option[String] = Some("An entry from the Git reflog")

}
