package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.os.{ ProcessResultClass, RunFunction }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }
import com.github.mdr.mash.subprocesses.ProcessRunner

object RunMethod extends MashMethod("run") {

  object Params {
    val Args = Parameter(
      nameOpt = Some("args"),
      summaryOpt = Some("Arguments to command"),
      isVariadic = true)
  }

  import Params._

  val params = ParameterModel(Seq(Args, RunFunction.Params.StandardIn))

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val args =
      target +: (boundParams(Args) match {
        case MashList(MashString(s, _)) ⇒ s.trim.split("\\s+").map(MashString(_)).toSeq
        case xs: MashList               ⇒ xs.elements
      })
    val stdinImmediateOpt = RunFunction.getStandardInOpt(boundParams)
    val result = ProcessRunner.runProcess(args, captureProcess = true, stdinImmediateOpt = stdinImmediateOpt)
    ProcessResultClass.fromResult(result)
  }

  override def typeInferenceStrategy = ProcessResultClass

  override def summaryOpt = Some("Execute the command at the given path, with the given arguments")

}
