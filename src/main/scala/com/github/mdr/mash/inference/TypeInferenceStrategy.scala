package com.github.mdr.mash.inference

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.ns.collections.MapFunction

import scala.language.implicitConversions

object TypeInferenceStrategy {

  implicit def constantTypeInferenceStrategy[T](klass: MashClass): ConstantTypeInferenceStrategy =
    ConstantTypeInferenceStrategy(Type.Instance(klass))

  implicit def constantTypeInferenceStrategy[T](type_ : Type): ConstantTypeInferenceStrategy =
    ConstantTypeInferenceStrategy(type_)

  implicit def constantTypeInferenceStrategy[T](classSeq: Seq[MashClass]): ConstantTypeInferenceStrategy =
    ConstantTypeInferenceStrategy(Type.Seq(classSeq.head))

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

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = Some(typ)

}

object SeqToSeqTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
    MapFunction.params.bindTypes(arguments).getType(MapFunction.Params.Sequence)

}