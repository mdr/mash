package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.subprocesses.ProcessResult
import scala.collection.immutable.ListMap
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.runtime.MashWrapped
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.ns.time.MillisecondsClass
import java.time.Instant
import java.time.Duration
import com.github.mdr.mash.evaluator.AbstractToStringMethod

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
    LineMethod,
    LinesMethod,
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
    val obj = x.asInstanceOf[MashObject]
    def stdout = obj(Stdout).asInstanceOf[MashString].s
    def line: String = stdout.split("\n").headOption.getOrElse("")
    def started = obj(Started).asInstanceOf[MashWrapped].x.asInstanceOf[Instant]
    def finished = obj(Finished).asInstanceOf[MashWrapped].x.asInstanceOf[Instant]
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
      val duration = Duration.between(wrapper.started, wrapper.finished)
      val millis = duration.getSeconds * 1000 + duration.getNano / 1000000
      MashNumber(millis, MillisecondsClass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(NumberClass taggedWith MillisecondsClass)

    override def summary = "How long the process ran for, in milliseconds"

  }

}