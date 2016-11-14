package com.github.mdr.mash.ns.time

import java.time._

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.runtime.MashWrapped

object NowFunction extends MashFunction("time.now") {

  private val clock: Clock = Clock.systemDefaultZone

  val params = ParameterModel()

  def apply(arguments: Arguments): MashWrapped = {
    params.validate(arguments)
    MashWrapped(clock.instant)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(DateTimeClass)

  override def summary = "The current date and time"

}
