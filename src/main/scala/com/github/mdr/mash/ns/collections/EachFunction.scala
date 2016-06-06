package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.MashUnit

object EachFunction extends MashFunction("collections.each") {

  object Params {
    val Action = Parameter(
      name = "action",
      summary = "Function used to act on elements of the sequence")
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to run an action over",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Action, Sequence))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    val action = boundParams.validateFunction(Action)
    sequence.foreach(action)
    MashUnit
  }

  override def typeInferenceStrategy = EachTypeInferenceStrategy

  override def summary = "Perform an action for each element in a sequence"

}

object EachTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    import EachFunction.Params._
    val argBindings = EachFunction.params.bindTypes(arguments)
    val sequenceExprOpt = argBindings.get(Sequence)
    val actionExprOpt = argBindings.get(Action)
    MapTypeInferenceStrategy.inferAppliedType(inferencer, actionExprOpt, sequenceExprOpt)
    Some(Type.Instance(UnitClass))
  }

}