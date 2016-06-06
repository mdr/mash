package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.utils.Utils
import com.github.mdr.mash.runtime.MashString

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
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isFlag = true,
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Sequence, Descending))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val descending = Truthiness.isTruthy(boundParams(Descending))
    val sorted = sequence.sorted(Utils.AnyOrdering)
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