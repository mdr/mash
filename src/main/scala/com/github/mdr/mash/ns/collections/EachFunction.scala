package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashNumber, MashUnit }
import com.github.mdr.mash.ns.collections.FlatMapFunction.zipWithMashIndex

object EachFunction extends MashFunction("collections.each") {

  object Params {
    val Action = Parameter(
      nameOpt = Some("action"),
      summaryOpt = Some("Function used to act on elements of the sequence"))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to run an action over"))
  }
  import Params._

  val params = ParameterModel(Action, Sequence)

  def call(boundParams: BoundParams): MashUnit = {
    val sequence = boundParams.validateSequence(Sequence)
    boundParams.validateFunction1Or2(Action) match {
      case Left(action)  ⇒ sequence.foreach(action)
      case Right(action) ⇒ zipWithMashIndex(sequence).foreach(action.tupled)
    }
    MashUnit
  }

  override def typeInferenceStrategy = EachTypeInferenceStrategy

  override def summaryOpt = Some("Perform an action for each element in a sequence")

}

object EachTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    import EachFunction.Params._
    val argBindings = EachFunction.params.bindTypes(arguments)
    val sequenceTypeOpt = argBindings.getType(Sequence)
    val actionExprOpt = argBindings.getArgument(Action)
    MapTypeInferenceStrategy.inferMappedType(inferencer, actionExprOpt, sequenceTypeOpt)
    Some(Unit)
  }

}