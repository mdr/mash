package com.github.mdr.mash.ns.os

import scala.collection.immutable.ListMap
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.parser.AbstractSyntax

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

}