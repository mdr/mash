package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.{ Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.ns.core.NumberClass

object TerminalClass extends MashClass("os.Terminal") {

  object Fields {
    val Rows = Field("rows", Some("Number of rows in the terminal"), NumberClass)
    val Columns = Field("columns", Some("Number of columns in the terminal"), NumberClass)
  }

  import Fields._

  override val fields = Seq(Rows, Columns)

  override val staticMethods = Seq(NewStaticMethod(this))

  override def summaryOpt = Some("Information about the terminal")

}

