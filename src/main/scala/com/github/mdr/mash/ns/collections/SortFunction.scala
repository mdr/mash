package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashBoolean, MashObject, MashValue, MashValueOrdering }
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

  object NaturalMashValueOrdering extends Ordering[MashValue] {

    override def compare(v1: MashValue, v2: MashValue): Int =
      SimpleNaturalComparator.getInstance[String].compare(ToStringifier.stringify(v1), ToStringifier.stringify(v2))

  }

  object NaturalStringOrdering extends Ordering[String] {
    override def compare(v1: String, v2: String): Int =
      SimpleNaturalComparator.getInstance[String].compare(v1, v2)
  }

  def call(boundParams: BoundParams): MashValue = {
    val sortable = boundParams(Sequence)
    val descending = boundParams(Descending).isTruthy
    val naturalOrder = boundParams(NaturalOrder).isTruthy
    sortable match {
      case obj: MashObject ⇒ sortObject(obj, descending, naturalOrder)
      case _               ⇒ sortSequence(sortable, boundParams, descending, naturalOrder)
    }
  }

  private def sortObject(obj: MashObject, descending: Boolean, naturalOrder: Boolean): MashValue = {
    val ordinaryOrdering = implicitly[Ordering[String]]
    val fieldOrdering = if (naturalOrder) NaturalStringOrdering else ordinaryOrdering
    val ordering = fieldOrdering.on[(String, MashValue)](_._1)
    val newFields = obj.immutableFields.toSeq.sortWith(ordering.lt).when(descending, _.reverse)
    MashObject.of(newFields)
  }

  private def sortSequence(inSequence: MashValue, boundParams: BoundParams, descending: Boolean, naturalOrder: Boolean): MashValue = {
    val sequence = boundParams.validateSequence(Sequence)
    val ordering: Ordering[MashValue] = if (naturalOrder) NaturalMashValueOrdering else MashValueOrdering
    val sorted = sequence.sortWith(ordering.lt)
    val newSequence = sorted.when(descending, _.reverse)
    WhereFunction.reassembleSequence(inSequence, newSequence)
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy

  override def summaryOpt = Some("Sort the elements of a sequence")

  override def descriptionOpt = Some(
    """If called on an object, the object fields are sorted.

Examples:
  sort [3, 1, 2]                                      # [1, 2, 3]
  sort --descending [3, 1, 2]                         # [3, 2, 1]
  sort --naturalOrder ["a2.txt", "a10.txt", "a1.txt"] # ["a1.txt", "a2.txt", "a10.txt"] """)

}