package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.objectClass.MapMethod
import com.github.mdr.mash.runtime._

object MapFunction extends MashFunction("collections.map") {

  object Params {
    val F = Parameter(
      nameOpt = Some("f"),
      summaryOpt = Some("Function used to transform elements of the sequence"),
      descriptionOpt = Some("If the function can take two arguments, the index is supplied as the second argument"))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to map over"))
  }

  import Params._

  val params = ParameterModel(F, Sequence)

  def call(boundParams: BoundParams): MashValue =
    boundParams(Sequence) match {
      case obj: MashObject ⇒
        MapMethod.doMap(obj, boundParams)
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
