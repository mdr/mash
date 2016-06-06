package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashValue

object TakeWhileFunction extends MashFunction("collections.takeWhile") {

  object Params {
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to take values from",
      isLast = true)
    val Predicate = Parameter(
      name = "predicate",
      summary = "Predicate used to test elements of the sequence")
  }
  import Params._

  val params = ParameterModel(Seq(Predicate, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val predicate = boundParams.validateFunction(Predicate)
    val newSequence = sequence.takeWhile(x ⇒ Truthiness.isTruthy(predicate(x)))
    WhereFunction.reassembleSequence(inSequence, newSequence)
  }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summary = "Take elements from the start of a sequence while a predicate holds"

  override def descriptionOpt = Some("""Examples:
  takeWhile (_ < 3) [1, 2, 3, 2, 1] # [1, 2]""")

}