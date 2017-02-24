package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.runtime.MashValue

object UniqueFunction extends MashFunction("collections.unique") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to remove duplicates from"),
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Sequence))

  def apply(boundParams: BoundParams): MashValue = {
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val newSequence = sequence.distinct
    WhereFunction.reassembleSequence(inSequence, newSequence)
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy

  override def summaryOpt = Some("Find all the distinct elements in a sequence")

  override def descriptionOpt = Some("""Examples:
  distinct [1, 2, 3, 2, 1] # [1, 2, 3]""")
}