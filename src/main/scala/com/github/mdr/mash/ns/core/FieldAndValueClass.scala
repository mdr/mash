package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.Field

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.inference.Type

object FieldAndValueClass extends MashClass("core.FieldAndValue") {

  object Fields {
    val Name = Field("name", "Field name", StringClass)
    val Value = Field("value", "Field value", Type.Any)

  }

  import Fields._

  override val fields = Seq(Name, Value)

  override def summary = "A field and its value"

}