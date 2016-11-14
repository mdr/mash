package com.github.mdr.mash.ns.os

import java.time.{ Duration, Instant }

import com.github.mdr.mash.evaluator.{ AbstractToStringMethod, Arguments, Field, MashClass }
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type }
import com.github.mdr.mash.ns.core.{ BooleanClass, NumberClass, StringClass }
import com.github.mdr.mash.ns.time.{ DateTimeClass, MillisecondsClass }
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.subprocesses.ProcessResult

import scala.collection.immutable.ListMap

object ProcessResultClass extends MashClass("os.ProcessResult") {

  object Fields {
    val ExitStatus = Field("exitStatus", "Exit status of process", NumberClass)
    val Stdout = Field("stdout", "Captured stdout", StringClass)
    val Started = Field("started", "Time process began running", DateTimeClass)
    val Finished = Field("finished", "Time process finished", DateTimeClass)
  }

  import Fields._

  override val fields = Seq(ExitStatus, Stdout, Started, Finished)

  def summary = "The result of running a process"

  override val methods = Seq(
    DurationMethod,
    FailedMethod,
    LineMethod,
    LinesMethod,
    SucceededMethod,
    ToNumberMethod,
    ToPathMethod,
    ToStringMethod)

  def fromResult(processResult: ProcessResult): MashObject = {
    import ProcessResultClass.Fields._
    MashObject.of(
      ListMap(
        ExitStatus -> MashNumber(processResult.exitStatus),
        Stdout -> MashString(processResult.stdout),
        Started -> MashWrapped(processResult.start),
        Finished -> MashWrapped(processResult.stop)),
      ProcessResultClass)
  }

  case class Wrapper(x: MashValue) {

    private val obj = x.asInstanceOf[MashObject]

    def stdout: String = obj.fieldAs[MashString](Stdout).s

    def started: Instant = obj.fieldAs[MashWrapped](Started).x.asInstanceOf[Instant]

    def finished: Instant = obj.fieldAs[MashWrapped](Finished).x.asInstanceOf[Instant]

    def line: String = stdout.split("\n").headOption.getOrElse("")

    def exitStatus: Int = obj.fieldAs[MashNumber](ExitStatus).asInt.get
  }

  object SucceededMethod extends MashMethod("succeeded") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      MashBoolean(Wrapper(target).exitStatus == 0)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "True if the status had a zero exit code"

  }

  object FailedMethod extends MashMethod("failed") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      MashBoolean(Wrapper(target).exitStatus != 0)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "True if the status had a non-zero exit code"

  }

  object LinesMethod extends MashMethod("lines") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      MashList(Wrapper(target).stdout.split("\n").map(MashString(_)))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Seq(Type.Instance(StringClass)))

    override def summary = "The standard output of the process as as sequence of lines"

  }

  object LineMethod extends MashMethod("line") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      MashString(Wrapper(target).line)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

    override def summary = "The first line of the standard output of the process (or empty, if no output)"

  }

  object ToNumberMethod extends MashMethod("toNumber") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      MashNumber(Wrapper(target).line.toDouble)
    }

    override def summary = "Parse the stdout of the process as a number"

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(NumberClass)

  }

  object ToPathMethod extends MashMethod("toPath") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      MashString(Wrapper(target).line, PathClass)
    }

    override def summary = "Tag the stdout as a path"

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass taggedWith PathClass)

  }

  object ToStringMethod extends AbstractToStringMethod {

    override def toString(target: MashValue) = Wrapper(target).stdout

  }

  object DurationMethod extends MashMethod("duration") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val wrapper = Wrapper(target)
      durationBetween(wrapper.started, wrapper.finished)
    }

    def durationBetween(started: Instant, finished: Instant): MashNumber = {
      val duration = Duration.between(started, finished)
      val millis = duration.getSeconds * 1000 + duration.getNano / 1000000
      MashNumber(millis, MillisecondsClass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(NumberClass taggedWith MillisecondsClass)

    override def summary = "How long the process ran for, in milliseconds"

  }

}