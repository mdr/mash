package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashNull, MashValue, MashValueOrdering }

object MinByFunction extends MashFunction("collections.minBy") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to find the minimum of"),
      isLast = true)
    val Attribute = Parameter(
      nameOpt = Some("attribute"),
      summaryOpt = Some("Function to extract a value to compare elements"))
  }
  import Params._

  val params = ParameterModel(Seq(Attribute, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    val attribute = boundParams.validateFunction(Attribute)

    var minValue: MashValue = null
    var minElem: MashValue = null
    var first = true
    for (elem <- sequence) {
      val value = attribute(elem)
      if (value != MashNull)
        if (first || MashValueOrdering.lt(value, minValue)) {
          minElem = elem
          minValue = value
          first = false
        }
    }
    if (first)
      boundParams.throwInvalidArgument(Sequence, "Cannot find minimum of an empty sequence")
    minElem
  }

  override def typeInferenceStrategy = FindTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summaryOpt = Some("Find the smallest element of a sequence by an attribute")

  override def descriptionOpt = Some("""The given function is applied to each element of the input sequence 
  to compute a value, and the element with the smallest value is returned. 
If there are multiple elements with the minimum value, the first is returned.

Examples:
  minBy length ["a", "bbb", "cc"] # "a"""")
}