package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashValue

import scala.PartialFunction.condOpt

object FlattenFunction extends MashFunction("collections.flatten") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence of sequences to flatten"),
      isLast = true)
  }

  import Params._

  val params = ParameterModel(Seq(Sequence))

  def call(boundParams: BoundParams): MashValue = {
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    FlatMapFunction.flatten(sequence, inSequence)
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