package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.runtime.MashNumber

object CountMatchesFunction extends MashFunction("collections.countMatches") {

  object Params {
    val Predicate = Parameter(
      nameOpt = Some("predicate"),
      summary = "Predicate used to test elements of the sequence")
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summary = "Sequence to count matches in",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Predicate, Sequence))

  def apply(arguments: Arguments): MashNumber = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    val predicate = boundParams.validateFunction(Predicate)
    val n = sequence.count(x ⇒ predicate(x).isTruthy)
    MashNumber(n)
  }

  override def typeInferenceStrategy = CountMatchesTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summary = "Count how many times a predicate holds within a sequence"

  override def descriptionOpt = Some("""Examples:
  countMatches (_ > 3) [1, 2, 3, 4, 5] # 2""")

}

object CountMatchesTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    import CountMatchesFunction.Params._
    val argBindings = CountMatchesFunction.params.bindTypes(arguments)
    val sequenceTypeOpt = argBindings.getType(Sequence)
    val predicateExprOpt = argBindings.getArgument(Predicate)
    MapTypeInferenceStrategy.inferMappedType(inferencer, predicateExprOpt, sequenceTypeOpt)
    Some(NumberClass)
  }

}
