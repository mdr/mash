package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashList, MashString, MashValue }

object ReverseFunction extends MashFunction("collections.reverse") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to reverse"),
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    boundParams.validateSequence(Sequence)
    boundParams(Sequence) match {
      case s: MashString ⇒ s.reverse
      case xs: MashList  ⇒ MashList(xs.elements.reverse)
    }
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy

  override def summaryOpt = Some("Reverse a sequence")

  override def descriptionOpt = Some("""Examples:
  reverse [1, 2, 3] # [3, 2, 1]
  reverse []        # []""")

}