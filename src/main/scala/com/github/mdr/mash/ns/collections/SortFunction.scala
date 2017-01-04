package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.{ Arguments, ToStringifier }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashBoolean, MashNull, MashValue, MashValueOrdering }
import net.greypanther.natsort.SimpleNaturalComparator

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
    val NaturalOrder = Parameter(
      name = "naturalOrder",
      shortFlagOpt = Some('n'),
      summary = "Use natural sort order ('alphanum') when comparing items",
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Sequence, Descending, NaturalOrder))

  object MashValueOrderingWithNullBottom extends Ordering[MashValue] {

    override def compare(v1: MashValue, v2: MashValue): Int = (v1, v2) match {
      case (MashNull, _) => -1
      case (_, MashNull) => 1
      case _             => MashValueOrdering.compare(v1, v2)
    }

  }

  object NaturalOrdering extends Ordering[MashValue] {

    override def compare(v1: MashValue, v2: MashValue): Int =
      SimpleNaturalComparator.getInstance[String].compare(ToStringifier.stringify(v1), ToStringifier.stringify(v2))

  }

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val descending = boundParams(Descending).isTruthy
    val naturalOrder = boundParams(NaturalOrder).isTruthy
    val sorted =
      if (naturalOrder)
        sequence.sortWith(NaturalOrdering.lt)
      else
        sequence.sortWith(MashValueOrderingWithNullBottom.lt)
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