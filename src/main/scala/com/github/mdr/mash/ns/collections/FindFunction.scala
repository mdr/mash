package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashNull, MashValue }

import scala.PartialFunction.condOpt

object FindFunction extends MashFunction("collections.find") {

  object Params {
    val Predicate = Parameter(
      nameOpt = Some("predicate"),
      summaryOpt = Some("Predicate used to test elements of the sequence"))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to search"))
  }

  import Params._

  val params = ParameterModel(Seq(Predicate, Sequence))

  def call(boundParams: BoundParams): MashValue = {
    val sequence = boundParams.validateSequence(Sequence)
    val predicate = boundParams.validateFunction(Predicate)
    sequence.find(x ⇒ predicate(x).isTruthy).getOrElse(MashNull)
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def typeInferenceStrategy = FindTypeInferenceStrategy

  override def summaryOpt = Some("Find an element in the sequence for which a predicate holds")

  override def descriptionOpt = Some(
    """Returns the first element of the sequence for which the predicate returns a truthy value.
null is returned if no matching element is found.

Examples:
  find (_ > 10) [1, 20, 3]  # 20
  find (_ > 100) [1, 2, 3]  # null""")

}

object FindTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = FindFunction.params.bindTypes(arguments)
    import FindFunction.Params._
    val sequenceTypeOpt = argBindings.getType(Sequence)
    val predicateExprOpt = argBindings.getArgument(Predicate)
    MapTypeInferenceStrategy.inferMappedType(inferencer, predicateExprOpt, sequenceTypeOpt)
    for {
      sequenceType ← sequenceTypeOpt
      elementType ← condOpt(sequenceType) {
        case Type.Seq(elementType)      ⇒ elementType
        case Type.Patterns.AnyString(_) ⇒ sequenceType
      }
    } yield elementType
  }

}