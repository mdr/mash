package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.runtime.MashBoolean

object AnyFunction extends MashFunction("collections.any") {

  val params = AllFunction.params

  import AllFunction.Params._

  def call(boundParams: BoundParams): MashBoolean = {
    val sequence = boundParams.validateSequence(Sequence)
    val predicate = boundParams.validateFunction(Predicate)
    MashBoolean(sequence.exists(x ⇒ predicate(x).isTruthy))
  }

  override def typeInferenceStrategy = AllTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summaryOpt = Some("Check whether a predicate holds for any element in a sequence")

  override def descriptionOpt = Some("""Returns true if the given predicate returns a truthy result for at least one element in the given sequence; false otherwise.

<mash>
Examples:
  any (_ > 0) [1, -2, 3]   # true
  any (_ > 0) [-1, -2, -3] # false
  any (_ > 0) []           # false
</mash>""")

}