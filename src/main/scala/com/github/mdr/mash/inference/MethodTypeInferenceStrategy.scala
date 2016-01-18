package com.github.mdr.mash.inference

import scala.PartialFunction.condOpt
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.ns.collections._

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
