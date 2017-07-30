package com.github.mdr.mash.ns.time

import java.time._

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.runtime.MashWrapped

object NowFunction extends MashFunction("time.now") {

  private val clock: Clock = Clock.systemDefaultZone

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashWrapped =
    MashWrapped(clock.instant)

  override def typeInferenceStrategy = DateTimeClass

  override def summaryOpt = Some("The current date and time")

}
