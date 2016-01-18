package com.github.mdr.mash.ns.time

import java.time._
import java.time.temporal.ChronoUnit

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type

abstract class ChronoUnitClass(name: String, unit: ChronoUnit) extends MashClass(name) {

  private val clock: Clock = Clock.systemDefaultZone

  override val methods = Seq(
    AgoMethod,
    FromNowMethod)

  object AgoMethod extends MashMethod("ago") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): Instant = {
      params.validate(arguments)
      val now = LocalDateTime.ofInstant(clock.instant, clock.getZone)
      val amount = target.asInstanceOf[MashNumber].n.toInt
      now.minus(amount, unit).atZone(clock.getZone).toInstant
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(DateTimeClass))

    override def summary = "Point in time this many " + unit.name.toLowerCase + " ago"

  }

  object FromNowMethod extends MashMethod("fromNow") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): Instant = {
      params.validate(arguments)
      val now = LocalDateTime.ofInstant(clock.instant, clock.getZone)
      val amount = target.asInstanceOf[MashNumber].n.toInt
      now.plus(amount, unit).atZone(clock.getZone).toInstant
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(DateTimeClass))

    override def summary = "Point in time this many " + unit.name.toLowerCase + " from now"

  }

  override def summary = "A tag class for a number of " + unit.name.toLowerCase

}

object DaysClass extends ChronoUnitClass("time.Days", ChronoUnit.DAYS)
object HoursClass extends ChronoUnitClass("time.Hours", ChronoUnit.HOURS)
object WeeksClass extends ChronoUnitClass("time.Weeks", ChronoUnit.WEEKS)
object MonthsClass extends ChronoUnitClass("time.Months", ChronoUnit.MONTHS)
