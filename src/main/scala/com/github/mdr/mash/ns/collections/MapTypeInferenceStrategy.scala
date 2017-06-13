package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.{ CharacterClass, StringClass }

import scala.PartialFunction.condOpt

object MapTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = MapFunction.params.bindTypes(arguments)
    import MapFunction.Params._
    val functionOpt = argBindings.getArgument(F)
    val sequenceTypeOpt = argBindings.getType(Sequence)
    val sequenceType = sequenceTypeOpt match {
      case Some(Type.Seq(elementType))               ⇒ elementType.seq
      case Some(Type.Patterns.AnyString(stringType)) ⇒ stringType
      case _                                         ⇒ Type.Any.seq
    }
    val newElementTypeOpt = inferMappedType(inferencer, functionOpt, Some(sequenceType))
    Some(getResultType(sequenceType, newElementTypeOpt))
  }

  private def getResultType(sequenceType: Type, newElementTypeOpt: Option[Type]): Type =
    sequenceType match {
      case Type.Patterns.AnyString(_) ⇒
        newElementTypeOpt match {
          case None | Some(Type.Patterns.AnyString(_)) ⇒ sequenceType
          case Some(newElementType)                    ⇒ Type.Seq(newElementType)
        }
      case _                          ⇒
        (newElementTypeOpt getOrElse Type.Any).seq
    }

  /**
    * Given a function, and a sequence,
    *
    * @return the type of the elements of the sequence after the mapping function has been applied.
    */
  def inferMappedType(inferencer: Inferencer,
                      functionExprOpt: Option[ValueInfo],
                      sequenceTypeOpt: Option[Type]): Option[Type] =
    for {
      ValueInfo(functionValueOpt, functionTypeOpt) ← functionExprOpt
      functionType ← functionTypeOpt
      sequenceType = sequenceTypeOpt getOrElse Type.Any.seq
      elementType ← condOpt(sequenceType) {
        case Type.Seq(elementType)               ⇒ elementType
        case Type.Patterns.AnyString(stringType) ⇒ StringClass taggedWith CharacterClass
        case _                                   ⇒ Type.Any
      }
      newElementType ← inferencer.applyFunction(functionType, elementType, functionValueOpt)
    } yield newElementType

}