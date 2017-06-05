package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashList, MashString, MashValue }

object ReverseFunction extends MashFunction("collections.reverse") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to reverse"))
  }
  import Params._

  val params = ParameterModel(Seq(Sequence))

  def call(boundParams: BoundParams): MashValue = {
    boundParams.validateSequence(Sequence)
    boundParams(Sequence) match {
      case s: MashString ⇒ s.reverse
      case xs: MashList  ⇒ MashList(xs.elements.reverse)
    }
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy(params, Sequence)

  override def summaryOpt = Some("Reverse a sequence")

  override def descriptionOpt = Some("""Examples:
  reverse [1, 2, 3] # [3, 2, 1]
  reverse []        # []""")

}