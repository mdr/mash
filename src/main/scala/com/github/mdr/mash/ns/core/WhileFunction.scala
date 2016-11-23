package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.runtime.MashUnit


object WhileFunction extends MashFunction("core.while") {

  object Params {
    val Condition = Parameter(
      name = "condition",
      summary = "Condition of the while loop",
      isLazy = true)
    val Block = Parameter(
      name = "block",
      summary = "Code to execute",
      isLazy = true)
  }

  import Params._

  val params = ParameterModel(Seq(Condition, Block))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val cond = boundParams(Condition).asInstanceOf[MashFunction]
    val block = boundParams(Block).asInstanceOf[MashFunction]
    while (cond.apply(Arguments(Seq())).isTruthy)
      block.apply(Arguments(Seq()))
    MashUnit
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Loop executing the given block while the condition remains true"

}