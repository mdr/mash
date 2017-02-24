package com.github.mdr.mash.ns.time

import java.time.{ Clock, LocalDateTime }

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.{ MashNumber, MashUnit }

object SleepFunction extends MashFunction("time.sleep") {

  private val clock: Clock = Clock.systemDefaultZone

  object Params {
    val Duration = Parameter(
      nameOpt = Some("duration"),
      summaryOpt = Some("Duration to sleep (default millisecond)"))
  }

  import Params._

  val params = ParameterModel(Seq(Duration))

  def apply(boundParams: BoundParams): MashUnit = {
    val millis = boundParams(Duration) match {
      case MashNumber(n, Some(klass: ChronoUnitClass)) ⇒
        val nowInstant = clock.instant
        val now = LocalDateTime.ofInstant(nowInstant, clock.getZone)
        val amount = klass.temporalAmount(n.toInt)
        val future = now.plus(amount).atZone(clock.getZone).toInstant
        future.toEpochMilli - nowInstant.toEpochMilli
      case MashNumber(n, _)                            ⇒
        n.toLong
      case x                                           ⇒
        boundParams.throwInvalidArgument(Duration, "Invalid duration of type " + x.typeName)
    }
    Thread.sleep(millis)
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Sleep for the given duration")

}
