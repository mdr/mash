package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashBoolean, MashValue, MashValueOrdering }
import com.github.mdr.mash.utils.Utils._
import net.greypanther.natsort.SimpleNaturalComparator

object SortFunction extends MashFunction("collections.sort") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to sort"),
      isLast = true)
    val Descending = Parameter(
      nameOpt = Some("descending"),
      shortFlagOpt = Some('d'),
      summaryOpt = Some("Sort results from highest value to lowest"),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)
    val NaturalOrder = Parameter(
      nameOpt = Some("naturalOrder"),
      shortFlagOpt = Some('n'),
      summaryOpt = Some("Use natural sort order ('alphanum') when comparing items"),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)
  }

  import Params._

  val params = ParameterModel(Seq(Sequence, Descending, NaturalOrder))

  object NaturalOrdering extends Ordering[MashValue] {

    override def compare(v1: MashValue, v2: MashValue): Int =
      SimpleNaturalComparator.getInstance[String].compare(ToStringifier.stringify(v1), ToStringifier.stringify(v2))

  }

  def call(boundParams: BoundParams): MashValue = {
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val descending = boundParams(Descending).isTruthy
    val naturalOrder = boundParams(NaturalOrder).isTruthy
    val ordering: Ordering[MashValue] = if (naturalOrder) NaturalOrdering else MashValueOrdering
    val sorted = sequence.sortWith(ordering.lt)

    val newSequence = sorted.when(descending, _.reverse)
    WhereFunction.reassembleSequence(inSequence, newSequence)
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy

  override def summaryOpt = Some("Sort the elements of a sequence")

  override def descriptionOpt = Some(
    """Examples:
  sort [3, 1, 2]                                      # [1, 2, 3]
  sort --descending [3, 1, 2]                         # [3, 2, 1]
  sort --naturalOrder ["a2.txt", "a10.txt", "a1.txt"] # ["a1.txt", "a2.txt", "a10.txt"] """)

}