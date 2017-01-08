package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime._

import scala.PartialFunction.condOpt

object MapFunction extends MashFunction("collections.map") {

  object Params {
    val F = Parameter(
      name = "f",
      summary = "Function used to transform elements of the sequence")
    val WithIndex = Parameter(
      name = "withIndex",
      shortFlagOpt = Some('i'),
      summary = "Pass index into the function as well as the item",
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to map over",
      isLast = true)
  }

  import Params._

  val params = ParameterModel(Seq(F, Sequence, WithIndex))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val withIndex = boundParams(WithIndex).isTruthy
    val sequence = boundParams.validateSequence(Sequence)
    val mapped =
      if (withIndex) {
        val f = boundParams.validateFunction2(F)
        sequence.zipWithIndex.map { case (x, i) ⇒ f(x, MashNumber(i)) }
      } else {
        val f = boundParams.validateFunction(F)
        sequence.map(f)
      }
    inSequence match {
      case MashString(_, tagOpt) if mapped.forall(_.isAString) ⇒
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
        Type.Seq(elementType) ← argBindings.getType(Sequence)
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
    val functionOpt = argBindings.getArgument(F)
    val sequenceTypeOpt = argBindings.getType(Sequence)
    val sequenceType = sequenceTypeOpt match {
      case Some(Type.Seq(elementType))                                                   ⇒ elementType.seq
      case Some(sequenceType@(Type.Instance(StringClass) | Type.Tagged(StringClass, _))) ⇒ sequenceType
      case _                                                                             ⇒ Type.Any.seq
    }
    val newElementTypeOpt = inferAppliedType(inferencer, functionOpt, Some(sequenceType))
    Some(getResultType(sequenceType, newElementTypeOpt))
  }

  private def getResultType(sequenceType: Type, newElementTypeOpt: Option[Type]): Type =
    sequenceType match {
      case Type.Instance(StringClass) | Type.Tagged(StringClass, _) ⇒
        newElementTypeOpt match {
          case None | Some(Type.Instance(StringClass) | Type.Tagged(StringClass, _)) ⇒ sequenceType
          case Some(newElementType)                                                  ⇒ Type.Seq(newElementType)
        }
      case _                                                        ⇒
        (newElementTypeOpt getOrElse Type.Any).seq
    }

  def inferAppliedType(inferencer: Inferencer, functionExprOpt: Option[ValueInfo], sequenceTypeOpt: Option[Type]): Option[Type] =
    for {
      ValueInfo(functionValueOpt, functionTypeOpt) ← functionExprOpt
      functionType ← functionTypeOpt
      sequenceType = sequenceTypeOpt getOrElse Type.Any.seq
      elementType ← condOpt(sequenceType) {
        case Type.Seq(elementType)                                    ⇒ elementType
        case Type.Instance(StringClass) | Type.Tagged(StringClass, _) ⇒ sequenceType
      }
      newElementType ← inferencer.applyFunction(functionType, elementType, functionValueOpt)
    } yield newElementType

}