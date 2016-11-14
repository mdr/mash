package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.{ MashList, MashString, MashValue }

import scala.PartialFunction.condOpt

object MapFunction extends MashFunction("collections.map") {

  object Params {
    val F = Parameter(
      name = "f",
      summary = "Function used to transform elements of the sequence")
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to map over",
      isLast = true)
  }

  import Params._

  val params = ParameterModel(Seq(F, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val f = boundParams.validateFunction(F)
    val mapped = sequence.map(f)
    inSequence match {
      case MashString(_, tagOpt) if mapped.forall(_.isInstanceOf[MashString]) ⇒
        mapped.asInstanceOf[Seq[MashString]].fold(MashString("", tagOpt))(_ + _)
      case _ ⇒
        MashList(mapped)
    }
  }

  override def typeInferenceStrategy = MapTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = {
    val argBindings = MapFunction.params.bindTypes(arguments)
    val specOpt =
      for {
        param ← argBindings.paramAt(argPos)
        if param == F
        AnnotatedExpr(_, Some(Type.Seq(elementType))) ← argBindings.get(Sequence)
      } yield CompletionSpec.Members(elementType)
    specOpt.toSeq
  }

  override def summary = "Transform each element of a sequence by a given function"

  override def descriptionOpt = Some("""The given function is applied to each element of the input sequence 
  to produce a sequence of transformed output elements.

Examples:
  map (_ * 2) [1, 2, 3] # [2, 4, 6]
  map (_ * 2) []        # []""")

}

object MapTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = MapFunction.params.bindTypes(arguments)
    import MapFunction.Params._
    val functionOpt = argBindings.get(F)
    val sequenceOpt = argBindings.get(Sequence)
    val newElementTypeOpt = inferAppliedType(inferencer, functionOpt, sequenceOpt)
    for {
      AnnotatedExpr(_, sequenceTypeOpt) ← sequenceOpt
      sequenceType ← sequenceTypeOpt
      newSequenceType ← condOpt(sequenceType) {
        case Type.Instance(StringClass) | Type.Tagged(StringClass, _) ⇒
          newElementTypeOpt match {
            case None | Some(Type.Instance(StringClass) | Type.Tagged(StringClass, _)) ⇒ sequenceType
            case Some(newElementType) ⇒ Type.Seq(newElementType)
          }
        case Type.Seq(_) ⇒
          newElementTypeOpt match {
            case Some(newElementType) ⇒ Type.Seq(newElementType)
            case None                 ⇒ Type.Seq(Type.Any)
          }
      }
    } yield newSequenceType
  }

  def inferAppliedType(inferencer: Inferencer, functionExprOpt: Option[AnnotatedExpr], sequenceExprOpt: Option[AnnotatedExpr]): Option[Type] = {
    for {
      AnnotatedExpr(functionExprOpt, functionTypeOpt) ← functionExprOpt
      functionType ← functionTypeOpt
      AnnotatedExpr(_, sequenceTypeOpt) ← sequenceExprOpt
      sequenceType ← sequenceTypeOpt
      elementType ← condOpt(sequenceType) {
        case Type.Seq(elementType)                                    ⇒ elementType
        case Type.Instance(StringClass) | Type.Tagged(StringClass, _) ⇒ sequenceType
      }
      newElementType ← inferencer.applyFunction(functionType, elementType, functionExprOpt)
    } yield newElementType
  }
}