package com.github.mdr.mash.ns.core

import java.time.Instant

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.Type.classToType
import com.github.mdr.mash.ns.time.{ DateTimeClass, MillisecondsClass }
import com.github.mdr.mash.runtime._

object TimedResultClass extends MashClass("core.TimedResult") {

  object Fields {
    val Result = Field("result", Some("Result of the piece of code executed"), Type.Any)
    val Started = Field("started", Some("Time execution started"), DateTimeClass)
    val Duration_ = Field("duration", Some("Duration of execution"), NumberClass taggedWith MillisecondsClass)
  }

  import Fields._

  override val fields = Seq(Result, Started, Duration_)

  override def summaryOpt = Some("The result of a timed execution")

  override val methods = Seq(FinishedMethod)

  override val staticMethods = Seq(NewStaticMethod(this))

  case class Wrapper(x: MashValue) extends AbstractObjectWrapper(x) {

    def started: Instant = target.fieldAs[MashWrapped](Started).x.asInstanceOf[Instant]

    def duration: Int = getNumberField(Duration_).toInt

  }

  object FinishedMethod extends MashMethod("finished") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashWrapped = {
      params.bindTo(arguments)
      val wrapper = Wrapper(target)
      val started = wrapper.started
      MashWrapped(started plusMillis wrapper.duration)
    }

    override def typeInferenceStrategy = DateTimeClass

    override def summaryOpt = Some("How long the process ran for, in milliseconds")

  }

}
