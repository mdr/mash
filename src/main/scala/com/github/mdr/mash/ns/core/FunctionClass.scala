package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.help.{ FunctionHelpClass, HelpFunction }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

object FunctionClass extends MashClass("core.Function") {

  override val methods = Seq(
    HelpMethod,
    InvokeMethod)

  object HelpMethod extends MashMethod("help") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      params.validate(arguments)
      HelpFunction.getHelp(target.asInstanceOf[MashFunction])
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(FunctionHelpClass)

    override def summary = "Help documentation for this function"
  }

  object InvokeMethod extends MashMethod("invoke") {

    object Params {
      val Args = Parameter(
        name = "args",
        summary = "Positional arguments for this function",
        defaultValueGeneratorOpt = Option(() ⇒ MashList.empty))
      val NamedArgs = Parameter(
        name = "namedArgs",
        summary = "Named arguments for this function",
        defaultValueGeneratorOpt = Option(() ⇒ MashObject.empty))
    }

    import Params._

    val params = ParameterModel(Seq(Args, NamedArgs))

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      val boundParams = params.validate(arguments)
      val args = boundParams.validateSequence(Args)
      val namedArgs = boundParams.validateObject(NamedArgs)
      val functionArguments = Arguments(args.map(v ⇒ EvaluatedArgument.PositionArg(SuspendedMashValue(() ⇒ v))) ++
      namedArgs.fields.toSeq.map { case (field, value) ⇒
        EvaluatedArgument.LongFlag(field, Some(SuspendedMashValue(() ⇒ value)))
      })
      target.asInstanceOf[MashFunction].apply(functionArguments)
    }

    override def summary = "Invoke this function with the given arguments"

  }

  override def summary = "A function"

  override def parentOpt = Some(AnyClass)

}