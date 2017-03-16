package com.github.mdr.mash.evaluator

import com.github.mdr.mash.classes.{ BoundMethod, MashClass }
import com.github.mdr.mash.evaluator.MemberEvaluator.MemberExprEvalResult
import com.github.mdr.mash.functions._
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime._

object InvocationEvaluator extends EvaluatorHelper {

  def evaluateInvocationExpr(invocationExpr: InvocationExpr)(implicit context: EvaluationContext) = {
    val InvocationExpr(functionExpr, arguments, _, _) = invocationExpr
    val evaluatedArguments = Arguments(arguments.map(evaluateArgument(_)))
    functionExpr match {
      case memberExpr: MemberExpr ⇒
        val MemberExprEvalResult(result, wasVectorised) = MemberEvaluator.evaluateMemberExpr(memberExpr,
          invokeNullaryWhenVectorising = false)
        if (wasVectorised) {
          val functions = result.asInstanceOf[MashList]
          functions.map(function ⇒ callFunction(function, evaluatedArguments, functionExpr, invocationExpr))
        } else
          callFunction(result, evaluatedArguments, functionExpr, invocationExpr)
      case _                      ⇒
        val function = Evaluator.simpleEvaluate(functionExpr)
        callFunction(function, evaluatedArguments, functionExpr, invocationExpr)
    }
  }

  def evaluateArgument(arg: Argument)(implicit context: EvaluationContext): EvaluatedArgument[SuspendedMashValue] = arg match {
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

  private def callFunction(function: MashValue, arguments: Arguments, functionExpr: Expr, invocationExpr: Expr): MashValue =
    callFunction(function, arguments, sourceLocation(functionExpr), sourceLocation(invocationExpr))

  def callFunction(function: MashValue,
                   arguments: Arguments,
                   functionLocationOpt: Option[SourceLocation] = None,
                   invocationLocationOpt: Option[SourceLocation] = None): MashValue =
    function match {
      case MashString(memberName, _)                  ⇒
        val f = new StringFunction(memberName, functionLocationOpt, invocationLocationOpt)
        val boundParams = translateArgumentException(invocationLocationOpt) {
          f.params.bindTo(arguments, EvaluationContext.NotUsed)
        }
        addInvocationToStackOnException(invocationLocationOpt, Some(f)) {
          f(boundParams)
        }
      case b: MashBoolean                             ⇒
        val f = new BooleanFunction(b.value)
        val boundParams = translateArgumentException(invocationLocationOpt) {
          f.params.bindTo(arguments, EvaluationContext.NotUsed)
        }
        addInvocationToStackOnException(invocationLocationOpt, Some(f)) {
          f(boundParams)
        }
      case function: MashFunction                     ⇒
        val boundParams = translateArgumentException(invocationLocationOpt) {
          function.params.bindTo(arguments, function.paramContext)
        }
        addInvocationToStackOnException(invocationLocationOpt, Some(function)) {
          function(boundParams)
        }
      case boundMethod@BoundMethod(target, method, _) ⇒
        val boundParams = translateArgumentException(invocationLocationOpt) {
          method.params.bindTo(arguments, method.paramContext(target))
        }
        addInvocationToStackOnException(invocationLocationOpt, Some(boundMethod)) {
          method(target, boundParams)
        }
      case klass: MashClass                           ⇒
        klass.getStaticMethod(MashClass.ConstructorMethodName) match {
          case Some(staticMethod) ⇒ callFunction(staticMethod, arguments, functionLocationOpt, invocationLocationOpt)
          case None               ⇒ throw new EvaluatorException(s"Value of type ${klass.typeName} is not callable", functionLocationOpt)
        }
      case x                                          ⇒
        throw new EvaluatorException(s"Value of type ${x.typeName} is not callable", functionLocationOpt)
    }

  private class BooleanFunction(b: Boolean) extends MashFunction() {

    object Params {
      val Then = Parameter(
        nameOpt = Some("then"),
        summaryOpt = Some("The result if this is true"),
        isLazy = true)
      val Else = Parameter(
        nameOpt = Some("else"),
        summaryOpt = Some("The result if this is false"),
        defaultValueGeneratorOpt = Some(MashUnit),
        isLazy = true)
    }

    import Params._

    val params = ParameterModel(Seq(Then, Else))

    def apply(boundParams: BoundParams): MashValue = {
      if (b)
        boundParams(Then).asInstanceOf[MashFunction].applyNullary()
      else
        boundParams(Else) match {
          case MashUnit        ⇒ MashUnit
          case f: MashFunction ⇒ f.applyNullary()
        }
    }

    override def summaryOpt = Some("Boolean as a function")

  }

  private class StringFunction(s: String,
                               functionLocationOpt: Option[SourceLocation],
                               invocationLocationOpt: Option[SourceLocation]) extends MashFunction() {

    object Params {
      val Target = Parameter(
        nameOpt = Some("target"),
        summaryOpt = Some("Member to look-up in the target object"))
    }

    import Params._

    val params = ParameterModel(Seq(Target))

    def apply(boundParams: BoundParams): MashValue = {
      boundParams(Target) match {
        case xs: MashList ⇒
          xs.map { target ⇒
            val intermediateResult = MemberEvaluator.lookup(target, s, locationOpt = functionLocationOpt)
            Evaluator.invokeNullaryFunctions(intermediateResult, invocationLocationOpt)
          }
        case v            ⇒
          val intermediateResult = MemberEvaluator.lookup(v, s, locationOpt = functionLocationOpt)
          Evaluator.invokeNullaryFunctions(intermediateResult, functionLocationOpt)
      }
    }

    override def summaryOpt = Some("String as a function")

  }

  def translateArgumentException[T](invocationLocationOpt: Option[SourceLocation])(p: ⇒ T): T =
    try
      p
    catch {
      case e: ArgumentException ⇒
        throw new EvaluatorException(e.message, e.locationOpt orElse invocationLocationOpt)
    }

  def addInvocationToStackOnException[T](invocationLocationOpt: Option[SourceLocation],
                                         functionOpt: Option[MashCallable] = None)(p: ⇒ T): T =
    try
      p
    catch {
      case e: ArgumentException  ⇒
        throw new EvaluatorException(e.message, e.locationOpt orElse invocationLocationOpt)
      case e: EvaluatorException ⇒
        throw invocationLocationOpt.map(e.lastWasFunction(functionOpt).push).getOrElse(e)
    }

}