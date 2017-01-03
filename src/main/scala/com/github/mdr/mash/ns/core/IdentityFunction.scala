package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
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

  override def typeInferenceStrategy = IdentityTypeInferenceStrategy

  override def summary = "Return the argument unchanged"

  override def descriptionOpt = Some("""Examples:
  identity 42 # 42""")

}

object IdentityTypeInferenceStrategy extends TypeInferenceStrategy {

  import IdentityFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
    IdentityFunction.params.bindTypes(arguments).getType(Item)

}