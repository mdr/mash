package com.github.mdr.mash.evaluator

import com.github.mdr.mash.classes.{ BoundMethod, MashClass }
import com.github.mdr.mash.evaluator.MemberEvaluator.MemberExprEvalResult
import com.github.mdr.mash.functions._
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime._

object InvocationEvaluator extends EvaluatorHelper {

  object MishFunction {

    object Params {
      val StandardIn = Parameter(
        nameOpt = Some("standardIn"),
        summaryOpt = Some("What to send to standard input"))
    }

    import Params._

    val params = ParameterModel(StandardIn)
  }

  def evaluateInvocationExpr(invocationExpr: InvocationExpr)(implicit context: EvaluationContext): MashValue = {
    val InvocationExpr(functionExpr, arguments, _, _) = invocationExpr
    val evaluatedArguments = Arguments(arguments.map(evaluateArgument(_)))
    functionExpr match {
      case mishExpr: MishExpr     ⇒
        val boundParams = translateArgumentException(sourceLocation(invocationExpr)) {
          MishFunction.params.bindTo(evaluatedArguments, context)
        }
        val stdinValue = boundParams(MishFunction.Params.StandardIn)
        MishEvaluator.evaluateMishExpr(mishExpr, Some(stdinValue))
      case memberExpr: MemberExpr ⇒
        val MemberExprEvalResult(result, wasVectorised) = MemberEvaluator.evaluateMemberExpr(memberExpr,
          invokeNullaryWhenVectorising = false)
        if (wasVectorised) {
          val functions = result.asInstanceOf[MashList]
          functions.map(function ⇒ callAsFunction(function, evaluatedArguments, functionExpr, invocationExpr))
        } else
          callAsFunction(result, evaluatedArguments, functionExpr, invocationExpr)
      case _                      ⇒
        val function = Evaluator.simpleEvaluate(functionExpr)
        callAsFunction(function, evaluatedArguments, functionExpr, invocationExpr)
    }
  }

  def evaluateArgument(arg: Argument)(implicit context: EvaluationContext): EvaluatedArgument[SuspendedMashValue] =
    arg match {
      case Argument.PositionArg(expr, sourceInfoOpt)        ⇒
        val suspendedValue = SuspendedMashValue(() ⇒ Evaluator.evaluate(expr))
        EvaluatedArgument.PositionArg(suspendedValue, Some(arg))
      case Argument.ShortFlag(flags, sourceInfoOpt)         ⇒
        EvaluatedArgument.ShortFlag(flags, Some(arg))
      case Argument.LongFlag(flag, valueOpt, sourceInfoOpt) ⇒
        val suspendedValueOpt = valueOpt.map { expr ⇒
          SuspendedMashValue(() ⇒ Evaluator.evaluate(expr))
        }
        EvaluatedArgument.LongFlag(flag, suspendedValueOpt, Some(arg))
    }

  private def callAsFunction(function: MashValue, arguments: Arguments, functionExpr: Expr, invocationExpr: Expr): MashValue =
    callAsFunction(function, arguments, sourceLocation(functionExpr), sourceLocation(invocationExpr))

  def callAsFunction(function: MashValue,
                     arguments: Arguments,
                     functionLocationOpt: Option[SourceLocation] = None,
                     invocationLocationOpt: Option[SourceLocation] = None): MashValue =
    function match {
      case MashString(s, _)         ⇒
        callStringAsFunction(s, arguments, functionLocationOpt, invocationLocationOpt)
      case b: MashBoolean           ⇒
        callBooleanAsFunction(b, arguments, invocationLocationOpt)
      case function: MashFunction   ⇒
        callFunction(function, arguments, invocationLocationOpt)
      case boundMethod: BoundMethod ⇒
        callBoundMethod(boundMethod, arguments, invocationLocationOpt)
      case klass: MashClass         ⇒
        callClassAsFunction(klass, arguments, invocationLocationOpt, functionLocationOpt)
      case obj: MashObject              ⇒
        callObjectAsFunction(obj, arguments, invocationLocationOpt, functionLocationOpt)
      case x ⇒
        throwValueIsNotCallableException(x, functionLocationOpt)
    }

  private def throwValueIsNotCallableException(x: MashValue, functionLocationOpt: Option[SourceLocation]): Nothing = {
    throw new EvaluatorException(s"Value of type ${x.typeName} is not callable", functionLocationOpt)
  }

  private def callBoundMethod(boundMethod: BoundMethod,
                              arguments: Arguments,
                              invocationLocationOpt: Option[SourceLocation]): MashValue = {
    val BoundMethod(target, method, _) = boundMethod
    val boundParams = translateArgumentException(invocationLocationOpt) {
      method.params.bindTo(arguments, method.paramContext(target))
    }
    addInvocationToStackOnException(invocationLocationOpt, Some(boundMethod)) {
      method.call(target, boundParams)
    }
  }

  private def callClassAsFunction(klass: MashClass,
                                  arguments: Arguments,
                                  invocationLocationOpt: Option[SourceLocation],
                                  functionLocationOpt: Option[SourceLocation]): MashValue =
    klass.getStaticMethod(MashClass.ConstructorMethodName) match {
      case Some(staticMethod) ⇒
        callAsFunction(staticMethod, arguments, functionLocationOpt, invocationLocationOpt)
      case None               ⇒
        throwValueIsNotCallableException(klass, functionLocationOpt)
    }

  private def callObjectAsFunction(obj: MashObject,
                                   arguments: Arguments,
                                   invocationLocationOpt: Option[SourceLocation],
                                   functionLocationOpt: Option[SourceLocation]): MashValue = {
    val f = new ObjectFunction(obj, functionLocationOpt, invocationLocationOpt)
    callFunction(f, arguments, invocationLocationOpt)
  }

  private def callFunction(function: MashFunction,
                           arguments: Arguments,
                           invocationLocationOpt: Option[SourceLocation]): MashValue = {
    val boundParams = translateArgumentException(invocationLocationOpt) {
      function.params.bindTo(arguments, function.paramContext)
    }
    addInvocationToStackOnException(invocationLocationOpt, Some(function)) {
      function.call(boundParams)
    }
  }

  private def callStringAsFunction(s: String,
                                   arguments: Arguments,
                                   functionLocationOpt: Option[SourceLocation],
                                   invocationLocationOpt: Option[SourceLocation]): MashValue = {
    val f = new StringFunction(s, functionLocationOpt, invocationLocationOpt)
    callFunction(f, arguments, invocationLocationOpt)
  }

  private def callBooleanAsFunction(b: MashBoolean,
                                    arguments: Arguments,
                                    invocationLocationOpt: Option[SourceLocation]): MashValue = {
    val f = new BooleanFunction(b.value)
    callFunction(f, arguments, invocationLocationOpt)
  }

  def translateArgumentException[T](invocationLocationOpt: Option[SourceLocation])(p: ⇒ T): T =
    try
      p
    catch {
      case e: ArgumentException ⇒
        throw new EvaluatorException(e.message, e.locationOpt orElse invocationLocationOpt)
    }

  def addInvocationToStackOnException[T](invocationLocationOpt: Option[SourceLocation] = None,
                                         functionOpt: Option[MashCallable] = None)(p: ⇒ T): T =
    try
      p
    catch {
      case e: ArgumentException  ⇒
        throw new EvaluatorException(e.message, e.locationOpt orElse invocationLocationOpt)
      case e: EvaluatorException ⇒
        throw e.lastWasFunction(functionOpt).push(invocationLocationOpt)
    }

}