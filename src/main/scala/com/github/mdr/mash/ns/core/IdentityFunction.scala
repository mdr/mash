package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashValue

object IdentityFunction extends MashFunction("core.identity") {

  object Params {
    val Item = Parameter(
      name = "item",
      summary = "Item to return")
  }
  import Params._

  val params = ParameterModel(Seq(Item))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    boundParams(Item)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(BooleanClass)

  override def summary = "Return the argument unchanged"

  override def descriptionOpt = Some("""Examples:
  identity 42 # 42""")

}
