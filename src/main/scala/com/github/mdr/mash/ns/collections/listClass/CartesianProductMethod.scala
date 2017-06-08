package com.github.mdr.mash.ns.collections.listClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, MethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.runtime.{ MashList, MashValue }

object CartesianProductMethod extends MashMethod("cartesianProduct") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Other sequence in the product"))
  }

  import Params._

  override val params = ParameterModel(Sequence)

  def call(target: MashValue, boundParams: BoundParams): MashList = {
    val sequence = boundParams.validateSequence(Sequence)
    MashList(
      for {
        element1 <- target.asInstanceOf[MashList].immutableElements
        element2 ← sequence
      } yield MashList.of(element1, element2))
  }

  override def summaryOpt: Option[String] = Some("Compute the cartesian product with another sequence")

}
