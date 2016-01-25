package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.os.linux.LinuxProcessInteractions
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import jnr.constants.platform.linux.Signal
import com.github.mdr.mash.functions.BoundParams

object KillFunction extends MashFunction("os.kill") {

  private val Signals: Map[String, Int] = {
    val pairs =
      for (signal ← Signal.values)
        yield signal.name.drop(3) -> signal.value
    pairs.toMap
  }

  private val processInteractions = LinuxProcessInteractions

  object Params {
    val Signal = Parameter(
      name = "signal",
      summary = "Signal to send to the process",
      defaultValueGeneratorOpt = Some(() ⇒ MashString("TERM", Some(SignalClass))),
      isFlag = true,
      isFlagValueMandatory = true,
      flagValueNameOpt = Some("signal"),
      descriptionOpt = Some("""The signal can either be given as a number, or by name (e.g. "KILL", "TERM", "HUP" etc).
The default signal is TERM."""))
    val Processes = Parameter(
      name = "processes",
      summary = "Processes to kill",
      isVariadic = true,
      descriptionOpt = Some("""Processes can be provided either as numbers (PIDs), or Process objects, or a sequence of numbers or Process objects."""))
  }

  val params = ParameterModel(Seq(Params.Signal, Params.Processes))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val signal = getSignal(boundParams, Params.Signal)
    val processes = boundParams.validateSequence(Params.Processes)
    if (processes.isEmpty)
      boundParams.throwInvalidArgument(Params.Processes, "must provide at least one process to kill")
    val pids = processes.flatMap(getPids)
    for (pid ← pids)
      processInteractions.kill(pid, signal)
  }

  def getSignal(boundParams: BoundParams, param: Parameter): Int =
    boundParams(param) match {
      case n: MashNumber ⇒ n.asInt.getOrElse(boundParams.throwInvalidArgument(param, "invalid signal: " + n))
      case s: MashString ⇒ Signals.get(s.s).getOrElse(boundParams.throwInvalidArgument(param, "invalid signal: " + s))
      case x             ⇒ boundParams.throwInvalidArgument(param, s"invalid signal '$x'")
    }

  private def getPids(x: Any): Seq[Int] = x match {
    case n: MashNumber                           ⇒ Seq(n.asInt.getOrElse(throw new EvaluatorException("Invalid process ID: " + n)))
    case obj @ MashObject(_, Some(ProcessClass)) ⇒ Seq(ProcessClass.Wrapper(obj).pid)
    case xs: MashList                            ⇒ xs.items.flatMap(getPids)
    case x                                       ⇒ throw new EvaluatorException("Invalid process ID: " + x)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def summary = "Send a signal to a process"

  override def descriptionOpt = Some("""Examples:
  ps | where (_.name.matches "java") | kill # Kill all Java processes
  kill --signal="HUP" 1280                  # Send the HUP signal to process with PID 1280
""")

}