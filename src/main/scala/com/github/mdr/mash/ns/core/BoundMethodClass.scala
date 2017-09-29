package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.{ BoundMethod, MashClass }
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.help.{ HelpCreator, MethodHelpClass }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }

object BoundMethodClass extends MashClass("core.BoundMethod") {

  override val methods = Seq(
    DescriptionMethod,
    HelpMethod,
    InvokeMethod,
    SummaryMethod,
    TargetMethod)

  object SummaryMethod extends MashMethod("summary") {

    val params = ParameterModel.Empty

    override def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val boundMethod = target.asInstanceOf[BoundMethod]
      MashString.maybe(boundMethod.method.summaryOpt)
    }

    override def summaryOpt: Option[String] = Some("Get the summary of this method, if any, else null")

    override def typeInferenceStrategy = StringClass

  }

  object DescriptionMethod extends MashMethod("description") {

    val params = ParameterModel.Empty

    override def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val boundMethod = target.asInstanceOf[BoundMethod]
      MashString.maybe(boundMethod.method.descriptionOpt)
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
      val args = boundParams.validateSequence(Args)
      val namedArgs = boundParams.validateObject(NamedArgs)
      val positionalArguments = args.map(v ⇒ EvaluatedArgument.PositionArg(SuspendedMashValue(() ⇒ v)))
      val namedArguments = namedArgs.immutableFields.toSeq.map { case (field, value) ⇒
        val argumentName = field match {
          case s: MashString ⇒ s.s
          case _             ⇒ throw EvaluatorException(s"Named arguments must be Strings, but was ${field.typeName}")
        }
        EvaluatedArgument.LongFlag(argumentName, Some(SuspendedMashValue(() ⇒ value)))
      }
      val methodArguments = Arguments(positionalArguments ++ namedArguments)
      val boundMethod = target.asInstanceOf[BoundMethod]
      val method = boundMethod.method
      val invocationBoundParams = method.params.bindTo(methodArguments, method.paramContext(target))
      InvocationEvaluator.addInvocationToStackOnException(functionOpt = Some(boundMethod)) {
        method.call(boundMethod.target, invocationBoundParams)
      }
    }

    override def summaryOpt = Some("Invoke this method with the given arguments")

  }

  object HelpMethod extends MashMethod("help") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashValue =
      HelpCreator.getHelp(target)

    override def typeInferenceStrategy = MethodHelpClass

    override def summaryOpt = Some("Help documentation for this method")

  }

  object TargetMethod extends MashMethod("target") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashValue = {
      target.asInstanceOf[BoundMethod].target
    }

    override def typeInferenceStrategy = (inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments) =>
      targetTypeOpt.collect {
        case Type.BoundBuiltinMethod(targetType, _) ⇒ targetType
      }

    override def summaryOpt = Some("Target of this bound method")

    override def descriptionOpt = Some(
      """Examples:
<mash>
  [1, 2, 3].sortBy.target # [1, 2, 3]
</mash>""")

  }

  override def summaryOpt = Some("A method bound to a target")

  override def parentOpt = Some(AnyClass)

}