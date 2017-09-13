package com.github.mdr.mash.ns.time

import java.time.{ Clock, LocalDateTime }

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.{ MashNumber, MashUnit }

object SleepFunction extends MashFunction("time.sleep") {

  private val clock: Clock = Clock.systemDefaultZone

  object Params {
    val Duration = Parameter(
      nameOpt = Some("duration"),
      summaryOpt = Some("Duration to sleep"),
      descriptionOpt = Some("Accepts tagged durations. If given an untagged Number, sleeps that number of milliseconds"))
  }

  import Params._

  val params = ParameterModel(Duration)

  def call(boundParams: BoundParams): MashUnit = {
    val duration = getDurationInMillis(boundParams)
    if (duration > 0)
      Thread.sleep(duration)
    MashUnit
  }

  private def getDurationInMillis(boundParams: BoundParams): Long = {
    boundParams(Duration) match {
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
  }

  override def typeInferenceStrategy = Unit

  override def summaryOpt = Some("Sleep for a given duration")

  override def descriptionOpt = Some(
    """Examples:
      |<mash>
      |  sleep 1000      # Sleep 1 second
      |  sleep 3.seconds
      |</mash>
    """.stripMargin)
}
