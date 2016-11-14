package com.github.mdr.mash.inference

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
