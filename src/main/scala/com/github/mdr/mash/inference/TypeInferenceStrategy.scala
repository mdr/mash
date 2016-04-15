package com.github.mdr.mash.inference

import com.github.mdr.mash.ns.collections.MapFunction

/**
 * Type inference strategy for functions
 */
trait TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type]

}

object NoTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = None

}

case class ConstantTypeInferenceStrategy(typ: Type) extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
    Some(typ)

}

object SeqToSeqTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = MapFunction.params.bindTypes(arguments)
    for {
      AnnotatedExpr(_, sequenceTypeOpt) ← argBindings.get(MapFunction.Params.Sequence)
      sequenceType ← sequenceTypeOpt
    } yield sequenceType
  }

}