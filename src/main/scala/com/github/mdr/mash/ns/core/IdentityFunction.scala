package com.github.mdr.mash.ns.core

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashValue

object IdentityFunction extends MashFunction("core.identity") {

  object Params {
    val Item = Parameter(
      nameOpt = Some("item"),
      summaryOpt = Some("Item to return"))
  }
  import Params._

  val params = ParameterModel(Seq(Item))

  def call(boundParams: BoundParams): MashValue = {
    boundParams(Item)
  }

  override def typeInferenceStrategy = IdentityTypeInferenceStrategy

  override def summaryOpt = Some("Return the argument unchanged")

  override def descriptionOpt = Some("""Examples:
  identity 42 # 42""")

}

object IdentityTypeInferenceStrategy extends TypeInferenceStrategy {

  import IdentityFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
    IdentityFunction.params.bindTypes(arguments).getType(Item)

}