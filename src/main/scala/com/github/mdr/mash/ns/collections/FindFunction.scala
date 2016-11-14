package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.{ MashNull, MashValue }

import scala.PartialFunction.condOpt

object FindFunction extends MashFunction("collections.find") {

  object Params {
    val Predicate = Parameter(
      name = "predicate",
      summary = "Predicate used to test elements of the sequence")
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to search",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Predicate, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    val predicate = boundParams.validateFunction(Predicate)
    sequence.find(x ⇒ predicate(x).isTruthy).getOrElse(MashNull)
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def typeInferenceStrategy = FindTypeInferenceStrategy

  override def summary = "Find an element in the sequence for which a predicate holds"

  override def descriptionOpt = Some("""Returns the first element of the sequence for which the predicate returns a truthy value.
null is returned if no matching element is found.
    
Examples:
  find (_ > 10) [1, 20, 3]  # 20
  find (_ > 100) [1, 2, 3]  # null""")

}

object FindTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = FindFunction.params.bindTypes(arguments)
    import FindFunction.Params._
    val sequenceExprOpt = argBindings.get(Sequence)
    val predicateExprOpt = argBindings.get(Predicate)
    MapTypeInferenceStrategy.inferAppliedType(inferencer, predicateExprOpt, sequenceExprOpt)
    for {
      sequenceExpr ← sequenceExprOpt
      sequenceType ← sequenceExpr.typeOpt
      elementType ← condOpt(sequenceType) {
        case Type.Seq(elementType)                                    ⇒ elementType
        case Type.Instance(StringClass) | Type.Tagged(StringClass, _) ⇒ sequenceType
      }
    } yield elementType
  }

}