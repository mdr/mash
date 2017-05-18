package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.{ CharacterClass, StringClass }
import com.github.mdr.mash.ns.core.objectClass.MapMethod
import com.github.mdr.mash.runtime._

import scala.PartialFunction.condOpt

object MapFunction extends MashFunction("collections.map") {

  object Params {
    val F = Parameter(
      nameOpt = Some("f"),
      summaryOpt = Some("Function used to transform elements of the sequence"),
      descriptionOpt = Some("If the function can take two arguments, the index is supplied as the second argument"))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to map over"),
      isLast = true)
  }

  import Params._

  val params = ParameterModel(Seq(F, Sequence))

  def call(boundParams: BoundParams): MashValue =
    boundParams(Sequence) match {
      case obj: MashObject ⇒ MapMethod.doMap(obj, boundParams)
      case inSequence      ⇒
        val sequence = boundParams.validateSequence(Sequence)
        val mapped: Seq[MashValue] =
          boundParams.validateFunction1Or2(F) match {
            case Left(f)  ⇒ sequence.map(f)
            case Right(f) ⇒ sequence.zipWithIndex.map { case (x, i) ⇒ f(x, MashNumber(i)) }
          }
        inSequence match {
          case MashString(_, tagOpt) if mapped.forall(_.isAString) ⇒
            mapped.asInstanceOf[Seq[MashString]].fold(MashString("", tagOpt))(_ + _)
          case _                                                   ⇒
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

  override def summaryOpt = Some("Transform each element of a sequence by a given function")

  override def descriptionOpt = Some(
    """The given function is applied to each element of the input sequence
  to produce a sequence of transformed output elements.

Examples:
  map (_ * 2) [1, 2, 3]             # [2, 4, 6]
  map (_ * 2) []                    # []
  map (v i ⇒ [v i]) ["a", "b", "c"] # [["a", 0], ["b", 1], ["c", 2]]""")

}

object MapTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = MapFunction.params.bindTypes(arguments)
    import MapFunction.Params._
    val functionOpt = argBindings.getArgument(F)
    val sequenceTypeOpt = argBindings.getType(Sequence)
    val sequenceType = sequenceTypeOpt match {
      case Some(Type.Seq(elementType))               ⇒ elementType.seq
      case Some(Type.Patterns.AnyString(stringType)) ⇒ stringType
      case _                                         ⇒ Type.Any.seq
    }
    val newElementTypeOpt = inferMappedType(inferencer, functionOpt, Some(sequenceType))
    Some(getResultType(sequenceType, newElementTypeOpt))
  }

  private def getResultType(sequenceType: Type, newElementTypeOpt: Option[Type]): Type =
    sequenceType match {
      case Type.Patterns.AnyString(_) ⇒
        newElementTypeOpt match {
          case None | Some(Type.Patterns.AnyString(_)) ⇒ sequenceType
          case Some(newElementType)                    ⇒ Type.Seq(newElementType)
        }
      case _                          ⇒
        (newElementTypeOpt getOrElse Type.Any).seq
    }

  /**
    * Given a function, and a sequence,
    *
    * @return the type of the elements of the sequence after the mapping function has been applied.
    */
  def inferMappedType(inferencer: Inferencer,
                      functionExprOpt: Option[ValueInfo],
                      sequenceTypeOpt: Option[Type]): Option[Type] =
    for {
      ValueInfo(functionValueOpt, functionTypeOpt) ← functionExprOpt
      functionType ← functionTypeOpt
      sequenceType = sequenceTypeOpt getOrElse Type.Any.seq
      elementType ← condOpt(sequenceType) {
        case Type.Seq(elementType)               ⇒ elementType
        case Type.Patterns.AnyString(stringType) ⇒ StringClass taggedWith CharacterClass
        case _                                   ⇒ Type.Any
      }
      newElementType ← inferencer.applyFunction(functionType, elementType, functionValueOpt)
    } yield newElementType

}