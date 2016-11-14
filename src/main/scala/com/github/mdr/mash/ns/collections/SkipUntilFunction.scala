package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashValue

object SkipUntilFunction extends MashFunction("collections.skipUntil") {

  val params = SkipWhileFunction.params

  import SkipWhileFunction.Params._

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val predicate = boundParams.validateFunction(Predicate)
    val newSequence = sequence.dropWhile(x ⇒ predicate(x).isFalsey)
    WhereFunction.reassembleSequence(inSequence, newSequence)
  }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summary = "Skip elements from the start of a sequence until a predicate holds"

  override def descriptionOpt = Some("""Examples:
  skipUntil (_ < 3) [1, 2, 3, 2, 1] # [2, 1]""")

}