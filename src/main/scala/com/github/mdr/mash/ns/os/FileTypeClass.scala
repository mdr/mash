package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.ns.core.AnyClass

object FileTypeClass extends MashClass("os.FileType") {

  object Values {
    val Dir = "dir"
    val File = "file"
    val Link = "link"
    val Other = "other"
  }

  import Values._

  override def enumerationValues: Option[Seq[String]] =
    Some(Seq(Dir, File, Link, Other))

  override def summary = "Type of filesystem object"

  override def parentOpt = Some(AnyClass)

}