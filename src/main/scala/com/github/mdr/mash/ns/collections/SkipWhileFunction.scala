package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashValue

object SkipWhileFunction extends MashFunction("collections.skipWhile") {

  object Params {
    val Predicate = Parameter(
      nameOpt = Some("predicate"),
      summaryOpt = Some("Predicate used to test elements of the sequence"))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to skip values from"))
  }
  import Params._

  val params = ParameterModel(Predicate, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val predicate = boundParams.validateFunction(Predicate)
    val newSequence = sequence.dropWhile(x ⇒ predicate(x).isTruthy)
    WhereFunction.reassembleSequence(inSequence, newSequence)
  }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summaryOpt = Some("Skip elements from the start of a sequence while a predicate holds")

  override def descriptionOpt = Some("""Examples:
<mash>
  skipWhile (_ < 3) [1, 2, 3, 2, 1] # [3, 2, 1]
</mash>""")

}