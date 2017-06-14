package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }

import scala.PartialFunction.condOpt

object FlattenFunction extends MashFunction("collections.flatten") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence of sequences to flatten"))
  }

  import Params._

  val params = ParameterModel(Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    flatten(sequence, inSequence)
  }

  def flatten(mappedValues: Seq[MashValue], inSequence: MashValue): MashValue =
    if (mappedValues.isEmpty)
      inSequence
    else if (mappedValues.forall(_.isAList))
      mappedValues.asInstanceOf[Seq[MashList]].fold(MashList.empty)(_ ++ _)
    else if (mappedValues.forall(_.isAnObject))
      mappedValues.asInstanceOf[Seq[MashObject]].fold(MashObject.empty)(_ + _)
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

  override def typeInferenceStrategy = FlattenTypeInferenceStrategy

  override def summaryOpt = Some("Flatten a sequence of sequences")

}

object FlattenTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = FlatMapFunction.params.bindTypes(arguments)
    import FlatMapFunction.Params._
    for {
      sequenceType ← argBindings.getType(Sequence)
      newSequenceType ← condOpt(sequenceType) {
        case Type.Seq(Type.Seq(x)) ⇒ Type.Seq(x)
      }
    } yield newSequenceType
  }

}