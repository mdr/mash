package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.ns.os.RunFunction
import com.github.mdr.mash.subprocesses.ProcessRunner

case class SystemCommandFunction(command: String) extends MashFunction(nameOpt = None) {

  private val Args = "args"

  def apply(arguments: Arguments) = {
    val boundParams = params.validate(arguments)
    val args = boundParams(Args) match {
      case MashList(xs: MashList) ⇒ xs.items
      case xs: MashList           ⇒ xs.items
      case x                      ⇒ Seq(x)
    }
    ProcessRunner.runProcess(MashString(command) +: args, expandTilde = true)
    ()
  }

  val params = ParameterModel(Seq(Parameter(Args, "Arguments", isVariadic = true)))

  override def summary = s"Call the system command $command"

}