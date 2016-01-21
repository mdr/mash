package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.evaluator.MashString

object SkipWhileFunction extends MashFunction("collections.skipWhile") {

  object Params {
    val Predicate = Parameter(
      name = "predicate",
      summary = "Predicate used to test elements of the sequence")
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to skip values from",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Predicate, Sequence))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val predicate = boundParams.validateFunction(Predicate)
    val newSequence = sequence.dropWhile(x ⇒ Truthiness.isTruthy(predicate(x)))
    inSequence match {
      case MashString(_, tagOpt) ⇒ newSequence.asInstanceOf[Seq[MashString]].fold(MashString("", tagOpt))(_ + _)
      case _                     ⇒ newSequence
    }

  }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summary = "Skip elements from the start of a sequence while a predicate holds"

  override def descriptionOpt = Some("""Examples:
  skipWhile (_ < 3) [1, 2, 3, 2, 1] # [3, 2, 1]""")

}