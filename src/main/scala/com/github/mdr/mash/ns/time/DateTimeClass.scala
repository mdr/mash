package com.github.mdr.mash.ns.time

import java.time._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.runtime.MashWrapped

object DateTimeClass extends MashClass("time.DateTime") {

  override val methods = Seq(DateMethod)

  object DateMethod extends MashMethod("date") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashWrapped = {
      params.validate(arguments)
      val instant = target.asInstanceOf[Instant]
      val localDateTime = LocalDateTime.ofInstant(instant, ZoneId.systemDefault)
      MashWrapped(localDateTime.toLocalDate)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(LocalDateClass)

    override def summary = "Date portion of this date and time"

  }

  override def summary = "An instant in time"

}

