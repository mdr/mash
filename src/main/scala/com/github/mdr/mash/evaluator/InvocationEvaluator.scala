package com.github.mdr.mash.evaluator

import com.github.mdr.mash.utils.PointedRegion
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.evaluator.MemberEvaluator.MemberExprEvalResult
import com.github.mdr.mash.functions.ArgumentException

object InvocationEvaluator extends EvaluatorHelper {

  def evaluateInvocationExpr(invocationExpr: InvocationExpr)(implicit context: EvaluationContext) = {
    val InvocationExpr(functionExpr, arguments, _, _) = invocationExpr
    val evaluatedArguments = Arguments(arguments.map(evaluateArgument(_)))
    functionExpr match {
      case memberExpr: MemberExpr ⇒
        val MemberExprEvalResult(result, wasVectorised) = MemberEvaluator.evaluateMemberExpr(memberExpr,
          immediatelyResolveNullaryWhenVectorising = false)
        if (wasVectorised) {
          val functions = result.asInstanceOf[MashList]
          functions.map(function ⇒ callFunction(function, evaluatedArguments, functionExpr, invocationExpr))
        } else
          callFunction(result, evaluatedArguments, functionExpr, invocationExpr)
      case _ ⇒
        val function = Evaluator.simpleEvaluate(functionExpr)
        callFunction(function, evaluatedArguments, functionExpr, invocationExpr)
    }
  }

  private def evaluateArgument(arg: Argument)(implicit context: EvaluationContext): EvaluatedArgument = arg match {
    case Argument.PositionArg(expr, sourceInfoOpt) ⇒
      EvaluatedArgument.PositionArg(Evaluator.evaluate(expr), Some(arg))
    case Argument.ShortFlag(flags, sourceInfoOpt) ⇒
      EvaluatedArgument.ShortFlag(flags, Some(arg))
    case Argument.LongFlag(flag, valueOpt, sourceInfoOpt) ⇒
      EvaluatedArgument.LongFlag(flag, valueOpt.map(v ⇒ Evaluator.evaluate(v)), Some(arg))
  }

  private def callFunction(function: MashValue, arguments: Arguments, functionExpr: Expr, invocationExpr: Expr)(implicit context: EvaluationContext): MashValue =
    callFunction(function, arguments, sourceLocation(functionExpr), sourceLocation(invocationExpr))

  def callFunction(function: MashValue,
                   arguments: Arguments,
                   functionLocationOpt: Option[SourceLocation] = None,
                   invocationLocationOpt: Option[SourceLocation] = None): MashValue =
    function match {
      case MashString(memberName, _) ⇒
        callStringAsFunction(memberName, arguments, functionLocationOpt, invocationLocationOpt)
      case f: MashFunction ⇒
        addInvocationToStackOnException(invocationLocationOpt, Some(f)) {
          f(arguments)
        }
      case BoundMethod(target, method, _) ⇒
        addInvocationToStackOnException(invocationLocationOpt, None) {
          method(target, arguments)
        }
      case x ⇒
        throw new EvaluatorException(s"Value of type ${x.typeName} is not callable", functionLocationOpt)
    }

  private def callStringAsFunction(s: String,
                                   arguments: Arguments,
                                   functionLocationOpt: Option[SourceLocation],
                                   invocationLocationOpt: Option[SourceLocation]): MashValue =
    (arguments.evaluatedArguments.collectFirst {
      case EvaluatedArgument.LongFlag(_, _, argumentNodeOpt) ⇒ argumentNodeOpt.flatMap(_.locationOpt)
      case EvaluatedArgument.ShortFlag(_, argumentNodeOpt)   ⇒ argumentNodeOpt.flatMap(_.locationOpt)
    }) match {
      case Some(locationOpt) ⇒
        throw new EvaluatorException(s"Cannot call a String with flag arguments", locationOpt)
      case None ⇒
        arguments.positionArgs match {
          case Seq(EvaluatedArgument.PositionArg(xs: MashList, _)) ⇒
            xs.map { target ⇒
              val intermediateResult = MemberEvaluator.lookup(target, s, functionLocationOpt)
              Evaluator.immediatelyResolveNullaryFunctions(intermediateResult, invocationLocationOpt)
            }
          case Seq(EvaluatedArgument.PositionArg(target, _)) ⇒
            val intermediateResult = MemberEvaluator.lookup(target, s, functionLocationOpt)
            Evaluator.immediatelyResolveNullaryFunctions(intermediateResult, functionLocationOpt)
          case Seq(_, second, _*) ⇒
            throw new EvaluatorException(s"Cannot call a String on multiple arguments", second.argumentNodeOpt.flatMap(_.locationOpt))
        }
    }

  def addInvocationToStackOnException[T](invocationLocationOpt: Option[SourceLocation], functionOpt: Option[MashFunction])(p: ⇒ T): T =
    try
      p
    catch {
      case e: ArgumentException ⇒
        throw new EvaluatorException(e.message, e.locationOpt orElse invocationLocationOpt)
      case e: EvaluatorException ⇒
        throw invocationLocationOpt.map(e.lastWasFunction(functionOpt).push).getOrElse(e)
    }

}