package com.github.mdr.mash.ns.os

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.NoArgFunction
import com.github.mdr.mash.ns.core.NoArgFunction._
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.subprocesses.ProcessRunner

object RunFunction extends MashFunction("os.run") {

  object Params {
    val StandardIn = Parameter(
      nameOpt = Some("standardIn"),
      summaryOpt = Some("What to send to the standard input of the process (default nothing)"),
      defaultValueGeneratorOpt = Some(NoArgValue),
      isFlag = true,
      isFlagValueMandatory = true)
    val Command = Parameter(
      nameOpt = Some("command"),
      summaryOpt = Some("Command to execute"),
      isVariadic = true,
      variadicAtLeastOne = true)
  }
  import Params._

  val params = ParameterModel(StandardIn, Command)

  def call(boundParams: BoundParams): MashObject = {
    val args: Seq[MashValue] =
      boundParams(Command) match {
        case MashList(xs: MashList)     ⇒ xs.immutableElements
        case MashList(MashString(s, _)) ⇒ s.trim.split("\\s+").map(MashString(_))
        case xs: MashList               ⇒ xs.elements
        case x                          ⇒ Seq(x)
      }
    val stdinImmediateOpt = getStandardInOpt(boundParams)
    if (args.isEmpty)
      throw new EvaluatorException("Must provide at least one argument for the command")
    val result = ProcessRunner.runProcess(args, captureProcess = true, stdinImmediateOpt = stdinImmediateOpt)
    ProcessResultClass.fromResult(result)
  }

  def getStandardInOpt(boundParams: BoundParams): Option[String] =
    NoArgFunction.option(boundParams(StandardIn)).map {
      case xs: MashList ⇒ xs.immutableElements.map(ToStringifier.stringify).mkString("\n")
      case x            ⇒ ToStringifier.stringify(x)
    }

  override def typeInferenceStrategy = ProcessResultClass

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Run a system command")

}