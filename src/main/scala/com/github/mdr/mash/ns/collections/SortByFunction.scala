package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.collections.SortFunction.NaturalMashValueOrdering
import com.github.mdr.mash.runtime._

object SortByFunction extends MashFunction("collections.sortBy") {

  object Params {
    val Attributes = Parameter(
      nameOpt = Some("attributes"),
      summaryOpt = Some("Function(s) to extract a value to compare elements"),
      isVariadic = true,
      variadicAtLeastOne = true)
  }

  import Params._
  import SortFunction.Params._

  val params = ParameterModel(Descending, NaturalOrder, Attributes, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val descending = boundParams(Descending).isTruthy
    val naturalOrder = boundParams(NaturalOrder).isTruthy
    val attributes: Seq[MashValue ⇒ MashValue] =
      boundParams.validateSequence(Attributes, allowStrings = false).map(boundParams.validateFunction(Attributes, _))
    val ordering: Ordering[MashValue] = if (naturalOrder) NaturalMashValueOrdering else MashValueOrdering

    val sorted = sequence.sortWith((a, b) ⇒ {
      val as = attributes.map(_ apply a).toList
      val bs = attributes.map(_ apply b).toList
      MashValueOrdering.compareLists(as, bs, ordering) < 0
    })
    val newSequence = if (descending) sorted.reverse else sorted
    WhereFunction.reassembleSequence(inSequence, newSequence)
  }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summaryOpt = Some("Sort the elements of a sequence by an attribute")

  override def descriptionOpt = Some(
    """Examples:
  sortBy length ["aa", "b", "ccc"]              # ["b", "aa", "ccc"] 
  sortBy --descending length ["aa", "b", "ccc"] # ["ccc", "aa", "b"]""")

}
