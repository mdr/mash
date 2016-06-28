package com.github.mdr.mash.ns.time

import java.time._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.ns.core.AnyClass

object DateTimeClass extends MashClass("time.DateTime") {

  override val methods = Seq(DateMethod)

  case class Wrapper(target: MashValue) {

    def instant: Instant = target match {
      case MashWrapped(i: Instant) ⇒ i
    }

  }

  object DateMethod extends MashMethod("date") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashWrapped = {
      params.validate(arguments)
      val instant = Wrapper(target).instant
      val localDateTime = LocalDateTime.ofInstant(instant, ZoneId.systemDefault)
      MashWrapped(localDateTime.toLocalDate)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(LocalDateClass)

    override def summary = "Date portion of this date and time"

  }

  override def summary = "An instant in time"

  override def parentOpt = Some(AnyClass)

}

