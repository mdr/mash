package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.TypedArguments

object AnyFunction extends MashFunction("collections.any") {

  val params = AllFunction.params

  import AllFunction.Params._

  def apply(arguments: Arguments): Boolean = {
    val boundParams = params.validate(arguments)
    val sequence = FunctionHelpers.interpretAsSequence(boundParams(Sequence))
    val predicate = FunctionHelpers.interpretAsFunction(boundParams(Predicate))
    sequence.exists(x ⇒ Truthiness.isTruthy(predicate(x)))
  }

  override def typeInferenceStrategy = AllTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summary = "Check whether a predicate holds for any element in a sequence"

  override def descriptionOpt = Some("""Returns true if the given predicate returns a truthy result for at least one element in the given sequence; false otherwise.
        
Examples:
  any (_ > 0) [1, -2, 3]   # true
  any (_ > 0) [-1, -2, -3] # false
  any (_ > 0) []           # false""")

}