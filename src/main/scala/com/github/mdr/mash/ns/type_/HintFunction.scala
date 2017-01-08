package com.github.mdr.mash.ns.type_

import com.github.mdr.mash.evaluator.{ Arguments, MashClass }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, Type, TypeInferenceStrategy, TypedArguments }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

import scala.PartialFunction.condOpt

object HintFunction extends MashFunction("type.hint") {

  object Params {
    val Hint = Parameter(
      name = "hint",
      summary = "Hint")
    val Item = Parameter(
      name = "item",
      summary = "Item to apply a type hint to")
  }

  import Params._

  val params = ParameterModel(Seq(Hint, Item))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    boundParams(Item)
  }

  override def typeInferenceStrategy = HintTypeInferenceStrategy

  override def summary = "Hint that the argument is a list"

}

object HintTypeInferenceStrategy extends TypeInferenceStrategy {

  import HintFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
    HintFunction.params
      .bindTypes(arguments)
      .getArgument(Hint)
      .flatMap(_.valueOpt)
      .flatMap(getType)

  private def getType(value: MashValue): Option[Type] = value match {
    case klass: MashClass    ⇒ Some(Type.Instance(klass))
    case MashList(listValue) ⇒ getType(listValue).map(_.seq)
    case obj: MashObject     ⇒
      val fieldTypes =
        for ((fieldName, fieldValue) <- obj.immutableFields)
          yield fieldName -> getType(fieldValue).getOrElse(Type.Instance(AnyClass))
      Some(Type.Object(fieldTypes))
    case _ ⇒
      None
  }

}