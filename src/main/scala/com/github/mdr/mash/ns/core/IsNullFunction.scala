package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashBoolean

object IsNullFunction extends MashFunction("core.isNull") {

  object Params {
    val Value = Parameter(
      name = "value",
      summary = "Value to test for nullness")
  }
  import Params._

  val params = ParameterModel(Seq(Value))

  def apply(arguments: Arguments): MashBoolean = {
    val boundParams = params.validate(arguments)
    MashBoolean(boundParams(Value) == MashNull)
  }

  override def summary = "Check whether or not the given argument is null"

  override def descriptionOpt = Some("""Examples:
  isNull null # true
  isNull 0    # false""")

}
