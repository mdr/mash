package com.github.mdr.mash.ns.core

import java.time.Instant

import com.github.mdr.mash.evaluator.{ Arguments, Field, MashClass }
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type }
import com.github.mdr.mash.inference.Type.classToType
import com.github.mdr.mash.ns.time.{ DateTimeClass, MillisecondsClass }
import com.github.mdr.mash.runtime._

object TimedResultClass extends MashClass("core.TimedResult") {

  object Fields {
    val Result = Field("result", "Result of the piece of code executed", Type.Any)
    val Started = Field("started", "Time execution started", DateTimeClass)
    val Duration_ = Field("duration", "Duration of execution", NumberClass taggedWith MillisecondsClass)
  }

  import Fields._

  override val fields = Seq(Result, Started, Duration_)

  override def summary = "The result of a timed execution"

  override val methods = Seq(FinishedMethod)

  case class Wrapper(x: MashValue) {

    private val obj = x.asInstanceOf[MashObject]

    def started: Instant = obj.fieldAs[MashWrapped](Started).x.asInstanceOf[Instant]

    def duration: Int = obj.fieldAs[MashNumber](Duration_).asInt.get

  }

  object FinishedMethod extends MashMethod("finished") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashWrapped = {
      params.validate(arguments)
      val wrapper = Wrapper(target)
      val started = wrapper.started
      MashWrapped(started plusMillis wrapper.duration)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(DateTimeClass)

    override def summary = "How long the process ran for, in milliseconds"

  }

}
