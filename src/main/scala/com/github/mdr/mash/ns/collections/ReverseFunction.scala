package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.evaluator.Arguments

object ReverseFunction extends MashFunction("collections.reverse") {

  object Params {
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to reverse",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Sequence))

  def apply(arguments: Arguments): Any = {
    params.validate(arguments)(Sequence) match {
      case s: MashString ⇒ s.reverse
      case xs: Seq[_]    ⇒ xs.reverse
    }
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy

  override def summary = "Reverse a sequence"

  override def descriptionOpt = Some("""Examples:
  reverse [1, 2, 3] # [3, 2, 1]
  reverse []        # []""")

}