package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashString, _ }

import scala.PartialFunction.condOpt

object FlatMapFunction extends MashFunction("collections.flatMap") {

  object Params {
    val F = Parameter(
      nameOpt = Some("f"),
      summaryOpt = Some("Function used to transform elements of the sequence"),
      descriptionOpt = Some("Must return a sequence. If the function can take two arguments, the index is supplied as the second argument."))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to map over"))
  }

  import Params._

  val params = ParameterModel(F, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val mapped: Seq[MashValue] =
      boundParams.validateFunction1Or2(F) match {
        case Left(f)  ⇒ sequence.map(f)
        case Right(f) ⇒ zipWithMashIndex(sequence).map(f.tupled)
      }
    flatten(mapped, inSequence)
  }

  private def zipWithMashIndex[T](items: Seq[T]): Seq[(T, MashNumber)] =
    items.zipWithIndex.map { case (item, index) ⇒ item -> MashNumber(index) }

  def flatten(mappedValues: Seq[MashValue], inSequence: MashValue): MashValue = {
    if (mappedValues.isEmpty)
      inSequence
    else if (mappedValues.forall(_.isAList))
      mappedValues.asInstanceOf[Seq[MashList]].fold(MashList.empty)(_ ++ _)
    else if (mappedValues.forall(_.isAString)) {
      val tagOpt = condOpt(inSequence) { case MashString(_, Some(tag)) ⇒ tag }
      mappedValues.asInstanceOf[Seq[MashString]].fold(MashString("", tagOpt))(_ + _)
    } else {
      val first = mappedValues.head // safe, mappedValues not empty
      val rest = mappedValues.tail
      val badItem =
        if (first.isAString)
          rest.find(x ⇒ !x.isAString).get // safe, because of above forall check
        else if (first.isAList)
          rest.find(x ⇒ !x.isAList).get // safe, because of above forall check
        else
          first
      throw new EvaluatorException("Invalid item of type " + badItem.typeName)

    }
  }

  override def typeInferenceStrategy = FlatMapTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = {
    val argBindings = FlatMapFunction.params.bindTypes(arguments)
    val specOpt =
      for {
        param ← argBindings.paramAt(argPos)
        if param == F
        Type.Seq(elementType) ← argBindings.getType(Sequence)
      } yield CompletionSpec.Members(elementType)
    specOpt.toSeq
  }

  override def summaryOpt = Some("Transform each element of a sequence by a given function, and then flatten")

  override def descriptionOpt = Some(
    """The given function is applied to each element of the input sequence
  and is expected to yield a sequence for each element. The result is flattened to produce a sequence of transformed 
  output elements.

Examples:
  flatMap (x => [x * 10, x * 100]) [1, 2, 3] # [20, 200, 40, 400, 60, 600]
  flatMap (n i => [n, i]) [1, 2, 3]          # [1, 0, 2, 1, 3, 2]""")

}

object FlatMapTypeInferenceStrategy extends TypeInferenceStrategy {

  import FlatMapFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = FlatMapFunction.params.bindTypes(arguments)
    val functionOpt = argBindings.getArgument(F)
    val sequenceTypeOpt = argBindings.getType(Sequence)
    val newElementSeqTypeOpt = MapTypeInferenceStrategy.inferMappedType(inferencer, functionOpt, sequenceTypeOpt)
    val newElementType = newElementSeqTypeOpt match {
      case Some(Type.Seq(elementType)) ⇒ elementType
      case _                           ⇒ Type.Any
    }
    Some(newElementType.seq)
  }

}