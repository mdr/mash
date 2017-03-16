package com.github.mdr.mash.ns.core

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashUnit


object WhileFunction extends MashFunction("core.while") {

  object Params {
    val Condition = Parameter(
      nameOpt = Some("condition"),
      summaryOpt = Some("Condition of the while loop"),
      isLazy = true)
    val Block = Parameter(
      nameOpt = Some("block"),
      summaryOpt = Some("Code to execute"),
      isLazy = true)
  }

  import Params._

  val params = ParameterModel(Seq(Condition, Block))

  def apply(boundParams: BoundParams): MashUnit = {
    val cond = boundParams(Condition).asInstanceOf[MashFunction]
    val block = boundParams(Block).asInstanceOf[MashFunction]
    while (cond.applyNullary().isTruthy)
      block.applyNullary()
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Loop executing the given block while the condition remains true")

}