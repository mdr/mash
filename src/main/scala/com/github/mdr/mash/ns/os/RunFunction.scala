package com.github.mdr.mash.ns.os

import scala.collection.JavaConverters._

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.subprocesses.ProcessRunner

object RunFunction extends MashFunction("os.run") {

  object Params {

    val Command = Parameter(
      name = "command",
      summary = "Command to execute",
      isVariadic = true)
  }
  import Params._

  val params = ParameterModel(Seq(Command))

  def apply(arguments: Arguments): MashNumber = {
    val boundParams = params.validate(arguments)
    val args: Seq[_] =
      boundParams(Command) match {
        case Seq(xs: Seq[_])       ⇒ xs
        case Seq(MashString(s, _)) ⇒ s.trim.split("\\s+").map(MashString(_))
        case xs: Seq[_]            ⇒ xs
        case x                     ⇒ Seq(x)
      }
    if (args.isEmpty)
      throw new EvaluatorException("Must provide at least one argument for the command")
    val statusCode = ProcessRunner.runProcess(args)
    MashNumber(statusCode)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(NumberClass))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Run a system command"

}