package com.github.mdr.mash.ns.os

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.subprocesses.ProcessRunner

object RunFunction extends MashFunction("os.run") {

  object Params {
    val Command = Parameter(
      nameOpt = Some("command"),
      summary = "Command to execute",
      isVariadic = true,
      variadicAtLeastOne = true)
    val StandardIn = Parameter(
      nameOpt = Some("standardIn"),
      summary = "What to send to standard input",
      defaultValueGeneratorOpt = Some(() ⇒ MashNull),
      isFlag = true,
      isFlagValueMandatory = true)
  }
  import Params._

  val params = ParameterModel(Seq(Command, StandardIn))

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val args: Seq[MashValue] =
      boundParams(Command) match {
        case MashList(xs: MashList)     ⇒ xs.elements
        case MashList(MashString(s, _)) ⇒ s.trim.split("\\s+").map(MashString(_))
        case xs: MashList               ⇒ xs.elements
        case x                          ⇒ Seq(x)
      }
    val stdinImmediateOpt = MashNull.option(boundParams(StandardIn)).map(ToStringifier.stringify)
    if (args.isEmpty)
      throw new EvaluatorException("Must provide at least one argument for the command")
    val result = ProcessRunner.runProcess(args, captureProcess = true, stdinImmediateOpt = stdinImmediateOpt)
    ProcessResultClass.fromResult(result)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(ProcessResultClass)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Run a system command"

}