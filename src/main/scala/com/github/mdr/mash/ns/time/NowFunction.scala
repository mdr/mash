package com.github.mdr.mash.ns.time

import java.time._

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type

object NowFunction extends MashFunction("time.now") {

  private val clock: Clock = Clock.systemDefaultZone

  val params = ParameterModel()

  def apply(arguments: Arguments): Instant = {
    params.validate(arguments)
    clock.instant
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(DateTimeClass)

  override def summary = "The current date and time"

}
