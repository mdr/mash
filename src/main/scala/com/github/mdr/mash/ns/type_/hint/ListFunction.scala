package com.github.mdr.mash.ns.type_.hint

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, Type, TypeInferenceStrategy, TypedArguments }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime.MashValue

object ListFunction extends MashFunction("type.hint.list") {

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

  override def typeInferenceStrategy = ListTypeInferenceStrategy

  override def summary = "Hint that the argument is a list"

}

object ListTypeInferenceStrategy extends TypeInferenceStrategy {

  import ListFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    ListFunction.params.bindTypes(arguments).getType(Item).collect {
      case Type.Seq(elementType) => Type.Seq(elementType)
    }.orElse(Some(Type.Seq(AnyClass)))
  }

}