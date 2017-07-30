package com.github.mdr.mash.ns.time

import java.time._
import java.time.format.{ DateTimeFormatter, FormatStyle }
import java.util.{ Date, Locale }

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.{ AnyClass, NumberClass, StringClass }
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.utils.TimeUtils
import org.ocpsoft.prettytime.PrettyTime

object DateTimeClass extends MashClass("time.DateTime") {

  override val methods = Seq(
    DateMethod,
    FormatMethod,
    FuzzyMethod,
    HourMethod,
    MillisSinceEpochMethod,
    MinuteMethod,
    SecondMethod)

  case class Wrapper(target: MashValue) {

    def instant: Instant = target match {
      case MashWrapped(i: Instant) ⇒ i
      case _                       ⇒ throw new EvaluatorException(s"Unexpected type: ${target.typeName}")
    }

    def localDateTime: LocalDateTime = TimeUtils.localDateTime(instant)

    def localDate: LocalDate = localDateTime.toLocalDate

  }

  object MillisSinceEpochMethod extends MashMethod("millisSinceEpoch") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashNumber =
      MashNumber(Wrapper(target).instant.toEpochMilli)

    override def typeInferenceStrategy = NumberClass

    override def summaryOpt = Some("Date portion of this date and time")

  }

  object FormatMethod extends MashMethod("format") {

    private val formatter =
      DateTimeFormatter.ofLocalizedDateTime(FormatStyle.LONG)
        .withLocale(Locale.getDefault).
        withZone(ZoneId.systemDefault())

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashString =
      MashString(formatter.format(Wrapper(target).instant))

    override def typeInferenceStrategy = StringClass

    override def summaryOpt = Some("Format as a human-friendly absolute time, e.g. June 30, 2017 6:29:09 PM BST")

  }

  object FuzzyMethod extends MashMethod("fuzzy") {
    private val prettyTime = new PrettyTime

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashString = {
      MashString(prettyTime.format(Date.from(Wrapper(target).instant)))
    }

    override def typeInferenceStrategy = StringClass

    override def summaryOpt = Some("Render as human-friendly relative time, e.g. 1 day ago, 3 months from now, etc")

  }

  object DateMethod extends MashMethod("date") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashWrapped =
      MashWrapped(Wrapper(target).localDate)

    override def typeInferenceStrategy = DateClass

    override def summaryOpt = Some("Date portion of this date and time")

  }

  object HourMethod extends MashMethod("hour") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashNumber =
      MashNumber(Wrapper(target).localDateTime.getHour)

    override def typeInferenceStrategy = NumberClass

    override def summaryOpt = Some("Hour of the day, in the current time zone")

  }

  object MinuteMethod extends MashMethod("minute") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashNumber =
      MashNumber(Wrapper(target).localDateTime.getMinute)

    override def typeInferenceStrategy = NumberClass

    override def summaryOpt = Some("Minute of the hour, in the current time zone")

  }

  object SecondMethod extends MashMethod("second") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashNumber =
      MashNumber(Wrapper(target).localDateTime.getSecond)

    override def typeInferenceStrategy = NumberClass

    override def summaryOpt = Some("Second of the minute, in the current time zone")

  }

  override def summaryOpt = Some("An instant in time")

  override def parentOpt = Some(AnyClass)

}

