package com.github.mdr.mash.ns.os

import java.time.{ Duration, Instant }

import com.github.mdr.mash.evaluator.{ AbstractToStringMethod, Arguments, Field, MashClass }
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type }
import com.github.mdr.mash.ns.core.{ BooleanClass, NumberClass, StringClass }
import com.github.mdr.mash.ns.time.{ DateTimeClass, MillisecondsClass }
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.subprocesses.ProcessResult
import com.github.mdr.mash.utils.StringUtils

import scala.collection.immutable.ListMap

object ProcessResultClass extends MashClass("os.ProcessResult") {

  object Fields {
    val ExitStatus = Field("exitStatus", Some("Exit status of process"), NumberClass)
    val Stdout = Field("stdout", Some("Captured stdout"), StringClass)
    val Started = Field("started", Some("Time process began running"), DateTimeClass)
    val Finished = Field("finished", Some("Time process finished"), DateTimeClass)
  }

  import Fields._

  override val fields = Seq(ExitStatus, Stdout, Started, Finished)

  override def summaryOpt = Some("The result of running a process")

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

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("True if the status had a zero exit code")

  }

  object FailedMethod extends MashMethod("failed") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      MashBoolean(Wrapper(target).exitStatus != 0)
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("True if the status had a non-zero exit code")

  }

  object LinesMethod extends MashMethod("lines") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      val stdout = Wrapper(target).stdout
      MashList(StringUtils.splitIntoLines(stdout).map(MashString(_)))
    }

    override def typeInferenceStrategy = Seq(StringClass)

    override def summaryOpt = Some("The standard output of the process as as sequence of lines")

  }

  object LineMethod extends MashMethod("line") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      MashString(Wrapper(target).line)
    }

    override def typeInferenceStrategy = StringClass

    override def summaryOpt = Some("The first line of the standard output of the process (or empty, if no output)")

  }

  object ToNumberMethod extends MashMethod("toNumber") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      MashNumber(Wrapper(target).line.toDouble)
    }

    override def summaryOpt = Some("Parse the stdout of the process as a number")

    override def typeInferenceStrategy = NumberClass

  }

  object ToPathMethod extends MashMethod("toPath") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      MashString(Wrapper(target).line, PathClass)
    }

    override def summaryOpt = Some("Tag the stdout as a path")

    override def typeInferenceStrategy = StringClass taggedWith PathClass

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

    override def typeInferenceStrategy = NumberClass taggedWith MillisecondsClass

    override def summaryOpt = Some("How long the process ran for, in milliseconds")

  }

}