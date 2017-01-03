package com.github.mdr.mash.inference

import scala.language.implicitConversions
import com.github.mdr.mash.ns.collections.MapFunction
import com.github.mdr.mash.evaluator.MashClass

object TypeInferenceStrategy {

  implicit def constantTypeInferenceStrategy[T](klass: MashClass): ConstantTypeInferenceStrategy =
    ConstantTypeInferenceStrategy(Type.Instance(klass))

}

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

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
    MapFunction.params.bindTypes(arguments).getType(MapFunction.Params.Sequence)

}