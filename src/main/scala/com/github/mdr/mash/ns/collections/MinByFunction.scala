package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.utils.Utils

object MinByFunction extends MashFunction("collections.minBy") {

  object Params {
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to find the minimum of",
      isLast = true)
    val Attribute = Parameter(
      name = "attribute",
      summary = "Function to extract a value to compare elements")
  }
  import Params._

  val params = ParameterModel(Seq(Attribute, Sequence))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    val sequence = FunctionHelpers.interpretAsSequence(boundParams(Sequence))
    val attribute = FunctionHelpers.interpretAsFunction(boundParams(Attribute))
    sequence.minBy(attribute)(Utils.AnyOrdering)
  }

  override def typeInferenceStrategy = FindTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summary = "Find the smallest element of a sequence by an attribute"

  override def descriptionOpt = Some("""The given function is applied to each element of the input sequence 
  to compute a value, and the element with the smallest value is returned. 
If there are multiple elements with the minimum value, the first is returned.

Examples:
  minBy length ["a", "bbb", "cc"] # "a"""")
}