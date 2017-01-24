package com.github.mdr.mash.ns.http

import com.github.mdr.mash.evaluator.{ Field, MashClass }
import com.github.mdr.mash.ns.core.StringClass

object HeaderClass extends MashClass("http.Header") {

  object Fields {
    lazy val Name = Field("name", Some("Header field name"), StringClass)
    lazy val Value = Field("body", Some("Header field value"), StringClass)
  }

  import Fields._

  override val fields = Seq(Name, Value)

  override def summary = "An HTTP header"
}