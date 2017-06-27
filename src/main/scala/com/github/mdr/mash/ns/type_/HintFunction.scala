package com.github.mdr.mash.ns.type_

import com.github.mdr.mash.classes.{ MashClass, UserDefinedClass }
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }

import scala.PartialFunction.condOpt

object HintFunction extends MashFunction("type.hint") {

  object Params {
    val Hint = Parameter(
      nameOpt = Some("hint"),
      summaryOpt = Some("Hint"))
    val Item = Parameter(
      nameOpt = Some("item"),
      summaryOpt = Some("Item to apply a type hint to"))
  }

  import Params._

  val params = ParameterModel(Hint, Item)

  def call(boundParams: BoundParams): MashValue = {
    boundParams(Item)
  }

  override def typeInferenceStrategy = HintTypeInferenceStrategy

  override def summaryOpt = Some("Hint that the argument is a list")

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

  private def getType(type_ : Type): Option[Type] = condOpt(type_) {
    case userClass: Type.UserClass ⇒ Type.UserClassInstance(userClass)
    case Type.Seq(elementType)     ⇒ getType(elementType).getOrElse(Type.Any).seq
  }

  private def getType(value: MashValue): Option[Type] = value match {
    case klass: UserDefinedClass ⇒ Some(new ValueTypeDetector().instanceType(klass))
    case klass: MashClass        ⇒ Some(Type.Instance(klass))
    case MashList(listValue)     ⇒ getType(listValue).map(_.seq)
    case obj: MashObject         ⇒
      val fieldTypes =
        for {
          (fieldName, fieldValue) <- obj.immutableFields
          stringFieldName ← condOpt(fieldName) { case s: MashString ⇒ s.s }
          fieldType = getType(fieldValue) getOrElse Type.Instance(AnyClass)
        } yield stringFieldName -> fieldType
      Some(Type.Object(fieldTypes))
    case _                       ⇒
      None
  }

}