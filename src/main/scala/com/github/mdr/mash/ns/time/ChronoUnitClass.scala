package com.github.mdr.mash.ns.time

import java.time._
import java.time.temporal.ChronoUnit
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashWrapped
import com.github.mdr.mash.runtime.MashValue
import java.time.temporal.TemporalAmount

abstract class ChronoUnitClass(name: String, unit: ChronoUnit) extends MashClass(name) {

  private val clock: Clock = Clock.systemDefaultZone

  override val methods = Seq(
    AgoMethod,
    FromNowMethod)

  object AgoMethod extends MashMethod("ago") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashWrapped = {
      params.validate(arguments)
      val now = LocalDateTime.ofInstant(clock.instant, clock.getZone)
      val amount = target.asInstanceOf[MashNumber].n.toInt
      MashWrapped(now.minus(temporalAmount(amount)).atZone(clock.getZone).toInstant)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(DateTimeClass)

    override def summary = "Point in time this many " + unit.name.toLowerCase + " ago"

  }

  object FromNowMethod extends MashMethod("fromNow") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashWrapped = {
      params.validate(arguments)
      val now = LocalDateTime.ofInstant(clock.instant, clock.getZone)
      val amount = target.asInstanceOf[MashNumber].n.toInt
      MashWrapped(now.plus(temporalAmount(amount)).atZone(clock.getZone).toInstant)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(DateTimeClass)

    override def summary = "Point in time this many " + unit.name.toLowerCase + " from now"

  }

  override def summary = "A tag class for a number of " + unit.name.toLowerCase

  def temporalAmount(n: Int): TemporalAmount

}

object MillisecondsClass extends ChronoUnitClass("time.Milliseconds", ChronoUnit.MILLIS) {
  override def temporalAmount(n: Int) = Duration.ofMillis(n)
}

object SecondsClass extends ChronoUnitClass("time.Seconds", ChronoUnit.SECONDS) {
  override def temporalAmount(n: Int) = Duration.ofSeconds(n)
}

object MinutesClass extends ChronoUnitClass("time.Minutes", ChronoUnit.MINUTES) {
  override def temporalAmount(n: Int) = Duration.ofMinutes(n)
}

object DaysClass extends ChronoUnitClass("time.Days", ChronoUnit.DAYS) {
  override def temporalAmount(n: Int) = Period.ofDays(n)
}

object HoursClass extends ChronoUnitClass("time.Hours", ChronoUnit.HOURS) {
  override def temporalAmount(n: Int) = Duration.ofHours(n)
}

object WeeksClass extends ChronoUnitClass("time.Weeks", ChronoUnit.WEEKS) {
  override def temporalAmount(n: Int) = Period.ofWeeks(n)
}

object MonthsClass extends ChronoUnitClass("time.Months", ChronoUnit.MONTHS) {
  override def temporalAmount(n: Int) = Period.ofMonths(n)
}
