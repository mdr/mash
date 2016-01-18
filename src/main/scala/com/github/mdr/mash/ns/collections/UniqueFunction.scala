package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy

object UniqueFunction extends MashFunction("collections.unique") {

  object Params {
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to remove duplicates from",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Sequence))

  def apply(arguments: Arguments): Seq[Any] = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams(Sequence).asInstanceOf[Seq[Any]]
    sequence.distinct
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy

  override def summary = "Find all the distinct elements in a sequence"

  override def descriptionOpt = Some("""Examples:
  distinct [1, 2, 3, 2, 1] # [1, 2, 3]""")
}