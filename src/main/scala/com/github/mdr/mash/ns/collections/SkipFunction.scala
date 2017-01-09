package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashList, MashNumber, MashString, MashValue }

object SkipFunction extends MashFunction("collections.skip") {

  object Params {
    val N = Parameter(
      nameOpt = Some("n"),
      summary = "Number of elements to skip (default 1)",
      defaultValueGeneratorOpt = Some(() ⇒ MashNumber(1)))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summary = "Sequence to skip values from",
      isLast = true)
  }

  import Params._

  val params = ParameterModel(Seq(N, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    boundParams.validateSequence(Sequence)
    val sequence = boundParams(Sequence)
    val n = boundParams.validateInteger(N)
    sequence match {
      case s: MashString ⇒ s.modify(_.drop(n))
      case xs: MashList  ⇒ MashList(xs.elements.drop(n))
    }
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy

  override def summary = "Skip elements from the start of a sequence"

  override def descriptionOpt = Some("""If there are fewer elements in the sequence than are requested to 
    skip, the empty sequence is returned.
    
Examples:
  skip 2 [1, 2, 3, 4] # [3, 4]
  skip 3 [1, 2]       # []""")

}