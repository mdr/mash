package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashNull, MashValue, MashValueOrdering }

object MaxByFunction extends MashFunction("collections.maxBy") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summary = "Sequence to find the maximum of",
      isLast = true)
    val Attribute = Parameter(
      nameOpt = Some("attribute"),
      summary = "Function to extract a value to compare elements")
  }
  import Params._

  val params = ParameterModel(Seq(Attribute, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    val attribute = boundParams.validateFunction(Attribute)

    var maxValue: MashValue = null
    var maxElem: MashValue = null
    var first = true
    for (elem <- sequence) {
      val value = attribute(elem)
      if (value != MashNull)
        if (first || MashValueOrdering.gt(value, maxValue)) {
          maxElem = elem
          maxValue = value
          first = false
        }
    }
    if (first)
      boundParams.throwInvalidArgument(Sequence, "Cannot find maximum of an empty sequence")
    maxElem
  }

  override def typeInferenceStrategy = FindTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summary = "Find the largest element of a sequence by an attribute"

  override def descriptionOpt = Some("""The given function is applied to each element of the input sequence 
  to compute a value, and the element with the largest value is returned. 
If there are multiple elements with the maximum value, the first is returned.

Examples:
  maxBy length ["a", "bbb", "cc"] # "bbb"""")

}