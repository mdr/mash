package com.github.mdr.mash.ns.random

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashNumber }

object IntFunction extends MashFunction("random.int") {

  object Params {
    val From = Parameter(
      nameOpt = Some("from"),
      summaryOpt = Some("Smallest number (inclusive)"),
      defaultValueGeneratorOpt = Some(MashNumber(0)))
    val To = Parameter(
      nameOpt = Some("to"),
      summaryOpt = Some("Largest number"))
    val Inclusive = Parameter(
      nameOpt = Some("inclusive"),
      shortFlagOpt = Some('i'),
      summaryOpt = Some("If true, generate integers including the 'to' value"),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)
  }

  import Params._

  val params = ParameterModel(Seq(From, To, Inclusive))

  def call(boundParams: BoundParams): MashNumber = {
    val from = boundParams.validateInteger(From)
    val to = boundParams.validateInteger(To)
    val inclusive = boundParams(Inclusive).isTruthy
    val range = to - from + (if (inclusive) 1 else 0)
    MashNumber((math.random() * range + from).toInt)
  }

  override def typeInferenceStrategy = NumberClass

  override def summaryOpt = Some("Generate a random integer in the given range")

}
