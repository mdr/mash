package com.github.mdr.mash.ns.type_

import com.github.mdr.mash.evaluator.{ Arguments, MashClass }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel, UserDefinedClass }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

import scala.PartialFunction.condOpt

object HintFunction extends MashFunction("type.hint") {

  object Params {
    val Hint = Parameter(
      nameOpt = Some("hint"),
      summary = "Hint")
    val Item = Parameter(
      nameOpt = Some("item"),
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

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val boundTypes = HintFunction.params.bindTypes(arguments)
    boundTypes.getArgument(Hint).flatMap(getType) orElse boundTypes.getType(Item)
  }

  private def getType(valueInfo: ValueInfo): Option[Type] =
    valueInfo.valueOpt.flatMap(getType) orElse
      valueInfo.typeOpt.flatMap(getType)

  private def getType(type_ : Type): Option[Type] = type_ match {
    case userClass: Type.UserClass ⇒ Some(Type.UserClassInstance(userClass))
    case Type.Seq(elementType)     ⇒ Some(getType(elementType).getOrElse(Type.Any).seq)
    case _                         ⇒ None
  }

  private def getType(value: MashValue): Option[Type] = value match {
    case klass: UserDefinedClass ⇒ Some(new ValueTypeDetector().instanceType(klass))
    case klass: MashClass        ⇒ Some(Type.Instance(klass))
    case MashList(listValue)     ⇒ getType(listValue).map(_.seq)
    case obj: MashObject         ⇒
      val fieldTypes =
        for ((fieldName, fieldValue) <- obj.immutableFields)
          yield fieldName -> getType(fieldValue).getOrElse(Type.Instance(AnyClass))
      Some(Type.Object(fieldTypes))
    case _                       ⇒
      None
  }

}