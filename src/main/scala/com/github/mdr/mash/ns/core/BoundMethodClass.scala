package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.{ BoundMethod, MashClass }
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.help.{ FunctionHelpClass, HelpCreator }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

object BoundMethodClass extends MashClass("core.BoundMethod") {

  override val methods = Seq(
    HelpMethod,
    TargetMethod,
    InvokeMethod)

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

    val params = ParameterModel(Seq(Args, NamedArgs))

    def apply(target: MashValue, boundParams: BoundParams): MashValue = {
      val args = boundParams.validateSequence(Args)
      val namedArgs = boundParams.validateObject(NamedArgs)
      val positionalArguments = args.map(v ⇒ EvaluatedArgument.PositionArg(SuspendedMashValue(() ⇒ v)))
      val namedArguments = namedArgs.fields.toSeq.map { case (field, value) ⇒
        EvaluatedArgument.LongFlag(field, Some(SuspendedMashValue(() ⇒ value)))
      }
      val methodArguments = Arguments(positionalArguments ++ namedArguments)
      val boundMethod = target.asInstanceOf[BoundMethod]
      val method = boundMethod.method
      val invocationBoundParams = method.params.bindTo(methodArguments, method.paramContext(target))
      method.apply(boundMethod.target, invocationBoundParams)
    }

    override def summaryOpt = Some("Invoke this method with the given arguments")

  }

  object HelpMethod extends MashMethod("help") {

    val params = ParameterModel()

    def apply(target: MashValue, boundParams: BoundParams): MashObject = {
      HelpCreator.getHelp(target)
    }

    override def typeInferenceStrategy = FunctionHelpClass

    override def summaryOpt = Some("Help documentation for this method")

  }

  object TargetMethod extends MashMethod("target") {

    val params = ParameterModel()

    def apply(target: MashValue, boundParams: BoundParams): MashValue = {
      target.asInstanceOf[BoundMethod].target
    }

    override def typeInferenceStrategy = (inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments) =>
      targetTypeOpt.collect {
        case Type.BoundBuiltinMethod(targetType, _) ⇒ targetType
      }

    override def summaryOpt = Some("Target of this bound method")

    override def descriptionOpt = Some(
      """Examples:
[1, 2, 3].sortBy.target # [1, 2, 3]""")

  }

  override def summaryOpt = Some("A method bound to a target")

  override def parentOpt = Some(AnyClass)

}