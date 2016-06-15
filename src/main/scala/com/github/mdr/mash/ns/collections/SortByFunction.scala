package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.utils.Utils
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.runtime.MashValueOrdering

object SortByFunction extends MashFunction(("collections.sortBy")) {

  object Params {
    val Descending = Parameter(
      name = "descending",
      shortFlagOpt = Some('d'),
      summary = "Sort results from highest value to lowest",
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)
    val Attribute = Parameter(
      name = "attribute",
      summary = "Function to extract a value to compare elements")
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to sort",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Descending, Attribute, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val descending = Truthiness.isTruthy(boundParams(Descending))
    val attribute = boundParams.validateFunction(Attribute)
    val sorted = sequence.sortBy(attribute)(MashValueOrdering)
    val newSequence = if (descending) sorted.reverse else sorted
    WhereFunction.reassembleSequence(inSequence, newSequence)
  }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summary = "Sort the elements of a sequence by an attribute"

  override def descriptionOpt = Some("""Examples:
  sortBy length ["aa", "b", "ccc"]              # ["b", "aa", "ccc"] 
  sortBy --descending length ["aa", "b", "ccc"] # ["ccc", "aa", "b"]""")

}