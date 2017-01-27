package com.github.mdr.mash.inference

import scala.language.implicitConversions
import com.github.mdr.mash.evaluator.MashClass

object MethodTypeInferenceStrategy {

  implicit def constantTypeInferenceStrategy[T](klass: MashClass): ConstantMethodTypeInferenceStrategy =
    ConstantMethodTypeInferenceStrategy(Type.Instance(klass))

  implicit def constantTypeInferenceStrategy[T](type_ : Type): ConstantMethodTypeInferenceStrategy =
    ConstantMethodTypeInferenceStrategy(type_)

  implicit def constantTypeInferenceStrategy[T](classSeq: Seq[MashClass]): ConstantMethodTypeInferenceStrategy =
    ConstantMethodTypeInferenceStrategy(Type.Seq(classSeq.head))

}

trait MethodTypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type]

}

object NoMethodTypeInferenceStrategy extends MethodTypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = None

}

case class ConstantMethodTypeInferenceStrategy(typ: Type) extends MethodTypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
    Some(typ)

}
