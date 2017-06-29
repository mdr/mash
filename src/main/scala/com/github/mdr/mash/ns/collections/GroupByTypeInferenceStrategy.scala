package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.ObjectClass

import scala.PartialFunction.condOpt

object GroupByTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    import GroupByFunction.Params._
    val argBindings = GroupByFunction.params.bindTypes(arguments)
    val sequenceTypeOpt = argBindings.getType(Sequence)
    val discriminatorExprOpt = argBindings.getArgument(Discriminator)
    val outputListOfGroups = argBindings.getArgument(Groups).flatMap(_.valueOpt).exists(_.isTruthy)
    if (outputListOfGroups)
      for {
        keyType ← MapTypeInferenceStrategy.inferMappedType(inferencer, discriminatorExprOpt, sequenceTypeOpt)
        sequenceType ← sequenceTypeOpt
        valuesType ← condOpt(sequenceType) {
          case Type.Seq(elementType)      ⇒ elementType
          case Type.Patterns.AnyString(_) ⇒ sequenceType
        }
      } yield Type.Seq(Type.Generic(GroupClass, keyType, valuesType))
    else
      Some(ObjectClass)
  }

}