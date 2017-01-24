package com.github.mdr.mash.ns.time

import java.time._

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.ns.time.DateTimeClass.DateMethod
import com.github.mdr.mash.runtime.MashWrapped

object TodayFunction extends MashFunction("time.today") {

  private val clock: Clock = Clock.systemDefaultZone

  val params = ParameterModel()

  def apply(arguments: Arguments): MashWrapped = {
    params.validate(arguments)
    MashWrapped(DateMethod.toLocalDate(clock.instant))
  }

  override def typeInferenceStrategy = DateClass

  override def summaryOpt = Some("The current date and time")

}
