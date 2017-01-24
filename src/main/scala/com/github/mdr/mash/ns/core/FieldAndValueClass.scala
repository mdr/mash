package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.{ Field, MashClass }
import com.github.mdr.mash.inference.Type

object FieldAndValueClass extends MashClass("core.FieldAndValue") {

  object Fields {
    val Name = Field("name", Some("Field name"), StringClass)
    val Value = Field("value", Some("Field value"), Type.Any)

  }

  import Fields._

  override val fields = Seq(Name, Value)

  override def summaryOpt = Some("A field and its value")

}