package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.collections.SortFunction.NaturalMashValueOrdering
import com.github.mdr.mash.ns.core.objectClass.SortByMethod
import com.github.mdr.mash.runtime._

object SortByFunction extends MashFunction("collections.sortBy") {

  object Params {
    val Attributes = Parameter(
      nameOpt = Some("attributes"),
      summaryOpt = Some("One or more functions to apply to elements to extract a value to compare"),
      descriptionOpt = Some(
        """If multiple attribute functions are supplied, elements are compared by the first attribute.
          |  If they are equal, then the second attribute is used to order them, and so on.""".stripMargin),
      isVariadic = true,
      variadicAtLeastOne = true)
  }

  import Params._
  import SortFunction.Params._

  val params = ParameterModel(Descending, NaturalOrder, Attributes, Sequence)

  def call(boundParams: BoundParams): MashValue =
    SequenceLikeAnalyser.analyse(boundParams, Sequence) {
      case list: SequenceLike.List     ⇒ list.reassemble(doSortBy(list.items, boundParams))
      case string: SequenceLike.String ⇒ string.reassemble(doSortBy(string.items, boundParams))
      case SequenceLike.Object(obj)    ⇒ SortByMethod.call(obj, boundParams)
    }

  private def doSortBy(items: Seq[MashValue], boundParams: BoundParams): Seq[MashValue] = {
    val descending = boundParams(Descending).isTruthy
    val naturalOrder = boundParams(NaturalOrder).isTruthy
    val attributes: Seq[MashValue ⇒ MashValue] =
      boundParams.validateSequence(Attributes, allowStrings = false).map(boundParams.validateFunction(Attributes, _))
    val ordering: Ordering[MashValue] = if (naturalOrder) NaturalMashValueOrdering else MashValueOrdering
    val sorted = items.sortWith((a, b) ⇒ {
      val as = attributes.map(_ apply a).toList
      val bs = attributes.map(_ apply b).toList
      MashValueOrdering.compareLists(as, bs, ordering) < 0
    })
    if (descending) sorted.reverse else sorted
  }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summaryOpt = Some("Sort the elements of a sequence by one or more attributes")

  override def descriptionOpt = Some(
    """Examples:
      |<mash>
      |  ["aa", "b", "ccc"] sortBy length                # ["b", "aa", "ccc"]
      |  ["aa", "b", "ccc"] | sortBy length --descending # ["ccc", "aa", "b"]
      |  ["az", "b", "aa"] | sortBy first last           # ["aa", "az", "b"]
      |</mash>""".stripMargin)

}
