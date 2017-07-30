package com.github.mdr.mash.ns.time

import java.time._

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.runtime.MashWrapped
import com.github.mdr.mash.utils.TimeUtils

object TodayFunction extends MashFunction("time.today") {

  private val clock: Clock = Clock.systemDefaultZone

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashWrapped =
    MashWrapped(TimeUtils.localDate(clock.instant))

  override def typeInferenceStrategy = DateClass

  override def summaryOpt = Some("The current date and time")

}
