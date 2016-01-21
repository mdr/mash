package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.ns.core.StringClass

object WhereFunction extends MashFunction("collections.where") {

  object Params {
    val Predicate = Parameter(
      name = "predicate",
      summary = "Predicate used to test elements of the sequence")
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to find values in",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Predicate, Sequence))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val predicate = boundParams.validateFunction(Predicate)
    val filtered = sequence.filter(x ⇒ Truthiness.isTruthy(predicate(x)))
    inSequence match {
      case MashString(_, tagOpt) ⇒ filtered.asInstanceOf[Seq[MashString]].fold(MashString("", tagOpt))(_ + _)
      case _                     ⇒ filtered
    }
  }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summary = "Find all the elements in the sequence for which a predicate holds"

  override def descriptionOpt = Some("""Examples:
  where (_ > 1) [1, 2, 3, 2, 1] # [2, 3, 2]""")

}

object WhereTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = WhereFunction.params.bindTypes(arguments)
    import WhereFunction.Params._
    val sequenceExprOpt = argBindings.get(Sequence)
    val predicateExprOpt = argBindings.get(Predicate)
    MapTypeInferenceStrategy.inferAppliedType(inferencer, predicateExprOpt, sequenceExprOpt)
    for {
      AnnotatedExpr(_, sequenceTypeOpt) ← sequenceExprOpt
      sequenceType ← sequenceTypeOpt
    } yield sequenceType
  }

}