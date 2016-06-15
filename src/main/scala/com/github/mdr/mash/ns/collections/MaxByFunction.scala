package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.utils.Utils
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.runtime.MashValueOrdering

object MaxByFunction extends MashFunction("collections.maxBy") {

  object Params {
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to find the maximum of",
      isLast = true)
    val Attribute = Parameter(
      name = "attribute",
      summary = "Function to extract a value to compare elements")
  }
  import Params._

  val params = ParameterModel(Seq(Attribute, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)   
    val attribute = boundParams.validateFunction(Attribute)
    sequence.maxBy(attribute)(MashValueOrdering)
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