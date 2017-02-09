package com.github.mdr.mash.ns.git

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass

object FileDiffClass extends MashClass("git.FileDiff") {

  object Fields {
    val _Type = Field("type", Some("Type of the change"), StringClass)
    val OldPath = Field("oldPath", Some("Old path"), StringClass taggedWith PathClass)
    val NewPath = Field("newPath", Some("Old path"), StringClass taggedWith PathClass)
  }
  import Fields._

  override lazy val fields = Seq(_Type, OldPath, NewPath)

  override lazy val methods = Seq()

  override def summaryOpt = Some("Summary of a change to a file")

}