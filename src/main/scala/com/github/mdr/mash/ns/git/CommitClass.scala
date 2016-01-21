package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.ns.os.PermissionsSectionClass
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.ns.time.DateTimeClass

object CommitClass extends MashClass("git.Commit") {

  object Fields {
    val Hash = Field("hash", "Commit hash", Type.Tagged(StringClass, CommitHashClass))
    val CommitTime = Field("commitTime", "Commit time", Type.Instance(DateTimeClass))
    val Author = Field("author", "Author of the commit", Type.Instance(StringClass))
    val Summary = Field("summary", "Summary message of the commit", Type.Instance(StringClass))
    val Parents = Field("parents", "Parents of this commit", Type.Seq(Type.Instance(CommitClass)))
  }

  import Fields._

  override lazy val fields = Seq(Hash, CommitTime, Author, Summary, Parents)

  def summary = "A git commit object"

}