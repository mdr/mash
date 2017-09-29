package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }

object FunctionClass extends MashClass("core.Function") {

  override val methods = Seq(
    DescriptionMethod,
    InvokeMethod,
    SummaryMethod)

  object SummaryMethod extends MashMethod("summary") {

    val params = ParameterModel.Empty

    override def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val f = target.asInstanceOf[MashFunction]
      MashString.maybe(f.summaryOpt)
    }

    override def summaryOpt: Option[String] = Some("Get the summary of this method, if any, else null")

    override def typeInferenceStrategy = StringClass

  }

  object DescriptionMethod extends MashMethod("description") {

    val params = ParameterModel.Empty

    override def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val f = target.asInstanceOf[MashFunction]
      MashString.maybe(f.descriptionOpt)
    }

    override def summaryOpt: Option[String] = Some("Get the description of this method, if any, else null")

    override def typeInferenceStrategy = StringClass

  }

  object InvokeMethod extends MashMethod("invoke") {

    object Params {
      val Args = Parameter(
        nameOpt = Some("args"),
        summaryOpt = Some("Positional arguments for this function"),
        defaultValueGeneratorOpt = Option(() ⇒ MashList.empty))
      val NamedArgs = Parameter(
        nameOpt = Some("namedArgs"),
        summaryOpt = Some("Named arguments for this function"),
        defaultValueGeneratorOpt = Option(() ⇒ MashObject.empty))
    }

    import Params._

    val params = ParameterModel(Args, NamedArgs)

    def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val function = target.asInstanceOf[MashFunction]
      val args = boundParams.validateSequence(Args)
      val namedArgs = boundParams.validateObject(NamedArgs)

      val positionalArguments = args.map(v ⇒ EvaluatedArgument.PositionArg(SuspendedMashValue(() ⇒ v)))
      val namedArguments = namedArgs.immutableFields.toSeq.map { case (field, value) ⇒
        val argumentName = field match {
          case s: MashString ⇒ s.s
          case _ ⇒ throw EvaluatorException(s"Named arguments must be Strings, but was ${field.typeName}")
        }
        EvaluatedArgument.LongFlag(argumentName, Some(SuspendedMashValue(() ⇒ value)))
      }
      val functionArguments = Arguments(positionalArguments ++ namedArguments)
      val functionBoundParams = function.params.bindTo(functionArguments, function.paramContext)
      InvocationEvaluator.addInvocationToStackOnException(functionOpt = Some(function)) {
        function.call(functionBoundParams)
      }
    }

    override def summaryOpt = Some("Invoke this function with the given arguments")

  }

  override def summaryOpt = Some("A function")

  override def parentOpt = Some(AnyClass)

}