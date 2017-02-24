package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashBoolean, MashNumber, MashUnit }

object EachFunction extends MashFunction("collections.each") {

  object Params {
    val Action = Parameter(
      nameOpt = Some("action"),
      summaryOpt = Some("Function used to act on elements of the sequence"))
    val WithIndex = Parameter(
      nameOpt = Some("withIndex"),
      shortFlagOpt = Some('i'),
      summaryOpt = Some("Pass index into the function as well as the item"),
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to run an action over"),
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Action, Sequence, WithIndex))

  def apply(boundParams: BoundParams): MashUnit = {
    val sequence = boundParams.validateSequence(Sequence)
    val withIndex = boundParams(WithIndex).isTruthy
    if (withIndex) {
      val action = boundParams.validateFunction2(Action)
      sequence.zipWithIndex.foreach { case (v, i) ⇒ action(v, MashNumber(i)) }
    } else {
      val action = boundParams.validateFunction(Action)
      sequence.foreach(action)
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