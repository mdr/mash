package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashNull, MashValue, MashValueOrdering }

object MaxByFunction extends MashFunction("collections.maxBy") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to find the maximum of"))
    val Attribute = Parameter(
      nameOpt = Some("attribute"),
      summaryOpt = Some("Function to extract a value to compare elements"))
    val Default = Parameter(
      nameOpt = Some("default"),
      summaryOpt = Some("Default value to return, if the items are empty"),
      defaultValueGeneratorOpt = Some(MashNull),
      isFlag = true,
      isFlagValueMandatory = true)
  }

  import Params._

  val params = ParameterModel(Attribute, Sequence, Default)

  def call(boundParams: BoundParams): MashValue = {
    val sequence = boundParams.validateSequence(Sequence)
    val attribute = boundParams.validateFunction(Attribute)
    val default = boundParams(Default)
    maxBy(sequence, attribute, default)
  }

  private def maxBy(sequence: Seq[MashValue], attribute: MashValue ⇒ MashValue, default: MashValue): MashValue = {
    var maxValue: MashValue = null
    var maxElem: MashValue = null
    var first = true
    for (elem ← sequence) {
      val value = attribute(elem)
      if (value != MashNull)
        if (first || MashValueOrdering.gt(value, maxValue)) {
          maxElem = elem
          maxValue = value
          first = false
        }
    }
    if (first)
      default
    else
      maxElem
  }

  override def typeInferenceStrategy = FindTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summaryOpt = Some("Find the largest element of a sequence by an attribute")

  override def descriptionOpt = Some(
    """The given function is applied to each element of the input sequence
  to compute a value, and the element with the largest value is returned. 
If there are multiple elements with the maximum value, the first is returned.

Examples:
  maxBy length ["a", "bbb", "cc"] # "bbb"""")

}