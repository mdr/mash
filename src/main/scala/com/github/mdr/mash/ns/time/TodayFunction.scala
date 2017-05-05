package com.github.mdr.mash.ns.time

import java.time._

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.time.DateTimeClass.DateMethod
import com.github.mdr.mash.runtime.MashWrapped

object TodayFunction extends MashFunction("time.today") {

  private val clock: Clock = Clock.systemDefaultZone

  val params = ParameterModel()

  def call(boundParams: BoundParams): MashWrapped =
    MashWrapped(DateMethod.toLocalDate(clock.instant))

  override def typeInferenceStrategy = DateClass

  override def summaryOpt = Some("The current date and time")

}
