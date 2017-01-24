package com.github.mdr.mash.ns.http

import com.github.mdr.mash.evaluator.{ Field, MashClass }
import com.github.mdr.mash.ns.core.StringClass

object CookieClass extends MashClass("http.Cookie") {

  object Fields {
    lazy val Name = Field("name", Some("Cookie name"), StringClass)
    lazy val Value = Field("body", Some("Cookie value"), StringClass)
  }

  import Fields._

  override val fields = Seq(Name, Value)

  override def summaryOpt = Some("An HTTP cookie")

}