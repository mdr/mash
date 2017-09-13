package com.github.mdr.mash.ns.os

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.os.linux.LinuxProcessInteractions
import com.github.mdr.mash.runtime._
import jnr.constants.platform.linux.Signal

object KillFunction extends MashFunction("os.kill") {

  private val Term = "TERM"

  private val Signals: Map[String, Int] = {
    val pairs =
      for (signal ← Signal.values)
        yield signal.name.drop(3) -> signal.value
    pairs.toMap
  }

  private val processInteractions = LinuxProcessInteractions

  object Params {
    val Signal = Parameter(
      nameOpt = Some("signal"),
      summaryOpt = Some("Signal to send to the process"),
      defaultValueGeneratorOpt = Some(MashString(Term, SignalClass)),
      isFlag = true,
      isFlagValueMandatory = true,
      flagValueNameOpt = Some("signal"),
      descriptionOpt = Some(
        """The signal can either be given as a number, or by name (e.g. "KILL", "TERM", "HUP" etc).
The default signal is TERM."""))
    val Processes = Parameter(
      nameOpt = Some("processes"),
      summaryOpt = Some("Processes to kill"),
      isVariadic = true,
      variadicAtLeastOne = true,
      descriptionOpt = Some("""Processes can be provided either as numeric PIDs or Process objects."""))
  }

  val params = ParameterModel(Params.Signal, Params.Processes)

  def call(boundParams: BoundParams): MashUnit = {
    val signal = getSignal(boundParams, Params.Signal)
    val processes = boundParams.validateSequence(Params.Processes)
    val pids = processes.map(getPid)
    for (pid ← pids)
      processInteractions.kill(pid, signal)
    MashUnit
  }

  def getSignal(boundParams: BoundParams, param: Parameter): Int =
    boundParams(param) match {
      case n: MashNumber ⇒ n.asInt.getOrElse(boundParams.throwInvalidArgument(param, "Invalid signal: " + n))
      case s: MashString ⇒ Signals.getOrElse(s.s, boundParams.throwInvalidArgument(param, "Invalid signal: " + s))
      case x             ⇒ boundParams.throwInvalidArgument(param, s"Invalid signal '$x'")
    }

  private def getPid(x: MashValue): Int =
    x match {
      case n: MashNumber                         ⇒ n.asInt.getOrElse(throw new EvaluatorException("Invalid process ID: " + n))
      case obj@MashObject(_, Some(ProcessClass)) ⇒ ProcessClass.Wrapper(obj).pid
      case _                                     ⇒ throw new EvaluatorException("Invalid process ID: " + x)
    }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Params.Signal ⇒ CompletionSpec.Items(SignalClass.Signals)
    }

  override def typeInferenceStrategy = Unit

  override def summaryOpt = Some("Send a signal to a process")

  override def descriptionOpt = Some(
    """Examples:
<mash>
  ps | grep "java" | kill   # Kill all Java processes
  kill --signal="HUP" 1280  # Send the HUP signal to process with PID 1280
</mash>""")

}