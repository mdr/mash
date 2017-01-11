package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.help.{ FunctionHelpClass, HelpFunction }
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
        summary = "Positional arguments for this function",
        defaultValueGeneratorOpt = Option(() ⇒ MashList.empty))
      val NamedArgs = Parameter(
        nameOpt = Some("namedArgs"),
        summary = "Named arguments for this function",
        defaultValueGeneratorOpt = Option(() ⇒ MashObject.empty))
    }

    import Params._

    val params = ParameterModel(Seq(Args, NamedArgs))

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      val boundParams = params.validate(arguments)
      val args = boundParams.validateSequence(Args)
      val namedArgs = boundParams.validateObject(NamedArgs)
      val methodArguments = Arguments(args.map(v ⇒ EvaluatedArgument.PositionArg(SuspendedMashValue(() ⇒ v))) ++
        namedArgs.fields.toSeq.map { case (field, value) ⇒
          EvaluatedArgument.LongFlag(field, Some(SuspendedMashValue(() ⇒ value)))
        })
      val boundMethod = target.asInstanceOf[BoundMethod]
      boundMethod.method.apply(boundMethod.target, methodArguments)
    }

    override def summary = "Invoke this method with the given arguments"

  }

  object HelpMethod extends MashMethod("help") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      params.validate(arguments)
      HelpFunction.getHelp(target.asInstanceOf[BoundMethod])
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(FunctionHelpClass)

    override def summary = "Help documentation for this method"

  }

  object TargetMethod extends MashMethod("target") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      target.asInstanceOf[BoundMethod].target
    }

    override def typeInferenceStrategy = new MethodTypeInferenceStrategy {
      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
        targetTypeOpt.collect {
          case Type.BoundBuiltinMethod(targetType, _) ⇒ targetType
        }
    }

    override def summary = "Target of this bound method"

    override def descriptionOpt = Some("""Examples:
[1, 2, 3].sortBy.target # [1, 2, 3]""")

  }

  override def summary = "A method bound to a target"

  override def parentOpt = Some(AnyClass)

}