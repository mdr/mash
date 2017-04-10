package com.github.mdr.mash.ns.collections.listClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, MethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.runtime.{ MashList, MashValue }

object IntersectMethod extends MashMethod("intersect") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Other sequence to intersect with this"))
  }

  import Params._

  override val params = ParameterModel(Seq(Sequence))

  def apply(target: MashValue, boundParams: BoundParams): MashList = {
    val sequence = MashList(boundParams.validateSequence(Sequence))
    target.asInstanceOf[MashList] intersect sequence
  }

  override def summaryOpt: Option[String] = Some("Compute the multiset intersection between this and another sequence")

  object IntersectMethodTypeInferenceStrategy extends MethodTypeInferenceStrategy {

    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
      val argBindings = params.bindTypes(arguments)
      targetTypeOpt orElse argBindings.getArgument(Sequence).flatMap(_.typeOpt)
    }
  }

  override def typeInferenceStrategy = IntersectMethodTypeInferenceStrategy
}
