package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashBoolean, MashValue, MashValueOrdering }

object SortFunction extends MashFunction("collections.sort") {

  object Params {
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to sort",
      isLast = true)
    val Descending = Parameter(
      name = "descending",
      shortFlagOpt = Some('d'),
      summary = "Sort results from highest value to lowest",
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Sequence, Descending))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val descending = boundParams(Descending).isTruthy
    val sorted = sequence.sorted(MashValueOrdering)
    val newSequence =
      if (descending)
        sorted.reverse
      else
        sorted
    WhereFunction.reassembleSequence(inSequence, newSequence)
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy

  override def summary = "Sort the elements of a sequence"

  override def descriptionOpt = Some("""Examples:
  sort [3, 1, 2]              # [1, 2, 3]
  sort --descending [3, 1, 2] # [3, 2, 1]""")

}