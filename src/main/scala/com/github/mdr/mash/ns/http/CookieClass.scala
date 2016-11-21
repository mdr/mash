package com.github.mdr.mash.ns.http

import com.github.mdr.mash.evaluator.{ Field, MashClass }
import com.github.mdr.mash.ns.core.StringClass

object CookieClass extends MashClass("http.Cookie") {

  object Fields {
    lazy val Name = Field("name", "Header field name", StringClass)
    lazy val Value = Field("body", "Header field value", StringClass)
  }

  import Fields._

  override val fields = Seq(Name, Value)

  override def summary = "An HTTP cookie"

}