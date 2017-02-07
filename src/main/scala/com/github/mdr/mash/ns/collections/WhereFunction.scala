package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.{ AnyClass, StringClass }
import com.github.mdr.mash.runtime.{ MashList, MashString, MashValue }

object WhereFunction extends MashFunction("collections.where") {

  object Params {
    val Predicate = Parameter(
      nameOpt = Some("predicate"),
      summaryOpt = Some("Predicate used to test elements of the sequence"))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to find values in"),
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Predicate, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val predicate = boundParams.validateFunction(Predicate)
    val newSequence = sequence.filter(x ⇒ predicate(x).isTruthy)
    reassembleSequence(inSequence, newSequence)
  }

  def reassembleSequence(inSequence: MashValue, newSequence: Seq[_ <: MashValue]): MashValue =
    inSequence match {
      case MashString(_, tagOpt) ⇒ newSequence.asInstanceOf[Seq[MashString]].fold(MashString("", tagOpt))(_ + _)
      case _                     ⇒ MashList(newSequence)
    }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summaryOpt = Some("Find all the elements in the sequence for which a predicate holds")

  override def descriptionOpt = Some("""Examples:
  where (_ > 1) [1, 2, 3, 2, 1] # [2, 3, 2]""")

}

object WhereTypeInferenceStrategy extends TypeInferenceStrategy {

  import WhereFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = WhereFunction.params.bindTypes(arguments)
    val sequenceTypeOpt = argBindings.getType(Sequence)
    val predicateExprOpt = argBindings.getArgument(Predicate)
    MapTypeInferenceStrategy.inferMappedType(inferencer, predicateExprOpt, sequenceTypeOpt)
    sequenceTypeOpt.collect {
      case typ@(Type.Patterns.AnyString(_) | Type.Seq(_)) ⇒ typ
    }.orElse(Some(Type.Seq(AnyClass)))
  }

}