package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashValue, _ }
import com.github.mdr.mash.utils.Utils._
import net.greypanther.natsort.SimpleNaturalComparator

object SortFunction extends MashFunction("collections.sort") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to sort"))
    val Descending = Parameter(
      nameOpt = Some("descending"),
      shortFlagOpt = Some('d'),
      summaryOpt = Some("Sort results from highest value to lowest"),
      defaultValueGeneratorOpt = Some(false),
      isFlag = true,
      isBooleanFlag = true)
    val NaturalOrder = Parameter(
      nameOpt = Some("naturalOrder"),
      shortFlagOpt = Some('n'),
      summaryOpt = Some("Use natural sort order ('alphanum') when comparing items"),
      defaultValueGeneratorOpt = Some(false),
      isFlag = true,
      isBooleanFlag = true)
  }

  import Params._

  val params = ParameterModel(Sequence, Descending, NaturalOrder)

  object NaturalMashValueOrdering extends Ordering[MashValue] {

    override def compare(v1: MashValue, v2: MashValue): Int =
      SimpleNaturalComparator.getInstance[String].compare(ToStringifier.stringify(v1), ToStringifier.stringify(v2))

  }

  def call(boundParams: BoundParams): MashValue = {
    val descending = boundParams(Descending).isTruthy
    val naturalOrder = boundParams(NaturalOrder).isTruthy
    SequenceLikeAnalyser.analyse(boundParams, Sequence) {
      case SequenceLike.List(items)          ⇒ sortItems(items, descending, naturalOrder)
      case sequenceLike: SequenceLike.String ⇒ sortString(sequenceLike, descending, naturalOrder)
      case SequenceLike.Object(obj)          ⇒ sortObject(obj, descending, naturalOrder)
    }
  }

  private def sortItems(items: Seq[MashValue], descending: Boolean, naturalOrder: Boolean): MashValue =
    MashList(sortItems_(items, descending, naturalOrder))

  private def sortString(sequenceLike: SequenceLike.String, descending: Boolean, naturalOrder: Boolean): MashValue = {
    val items = sequenceLike.items
    val newSequence = sortItems_(items, descending, naturalOrder)
    sequenceLike.reassemble(newSequence)
  }

  private def sortItems_(items: Seq[MashValue], descending: Boolean, naturalOrder: Boolean): Seq[MashValue] = {
    val ordering = if (naturalOrder) NaturalMashValueOrdering else MashValueOrdering
    val sorted = items sortWith ordering.lt
    sorted.when(descending, _.reverse)
  }

  private def sortObject(obj: MashObject, descending: Boolean, naturalOrder: Boolean): MashValue = {
    val fieldOrdering = if (naturalOrder) NaturalMashValueOrdering else MashValueOrdering
    val ordering = fieldOrdering.on[(MashValue, MashValue)](_._1)
    val newFields = obj.immutableFields.toSeq.sortWith(ordering.lt).when(descending, _.reverse)
    MashObject.of(newFields)
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy(params, Sequence)

  override def summaryOpt = Some("Sort the elements of a sequence")

  override def descriptionOpt = Some(
    """If called on an object, the object fields are sorted.
Examples:
<mash>
  sort [3, 1, 2]                                      # [1, 2, 3]
  sort --descending [3, 1, 2]                         # [3, 2, 1]
  sort --naturalOrder ["a2.txt", "a10.txt", "a1.txt"] # ["a1.txt", "a2.txt", "a10.txt"]
</mash>""")

}