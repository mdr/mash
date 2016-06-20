package com.github.mdr.mash.evaluator

import com.github.mdr.mash.utils.PointedRegion
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.evaluator.MemberEvaluator.MemberExprEvalResult

object InvocationEvaluator {

  def evaluateInvocationExpr(invocationExpr: InvocationExpr)(implicit context: EvaluationContext) = {
    val InvocationExpr(functionExpr, arguments, _) = invocationExpr
    val evaluatedArguments = Arguments(arguments.map(evaluateArgument(_)))
    functionExpr match {
      case memberExpr: MemberExpr ⇒
        val MemberExprEvalResult(result, wasVectorised) = MemberEvaluator.evaluateMemberExpr(memberExpr, immediatelyResolveNullaryWhenVectorising = false)
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
    case Argument.PositionArg(expr, sourceInfoOpt)        ⇒ EvaluatedArgument.PositionArg(Evaluator.evaluate(expr), Some(arg))
    case Argument.ShortFlag(flags, sourceInfoOpt)         ⇒ EvaluatedArgument.ShortFlag(flags, Some(arg))
    case Argument.LongFlag(flag, valueOpt, sourceInfoOpt) ⇒ EvaluatedArgument.LongFlag(flag, valueOpt.map(v ⇒ Evaluator.evaluate(v)), Some(arg))
  }

  private def callFunction(function: MashValue, arguments: Arguments, functionExpr: Expr, invocationExpr: Expr): MashValue =
    callFunction(function, arguments, Some(functionExpr), Some(invocationExpr))

  def callFunction(function: MashValue, arguments: Arguments, functionExprOpt: Option[Expr] = None, invocationExprOpt: Option[Expr] = None): MashValue = {
    val functionLocationOpt = functionExprOpt.flatMap(_.locationOpt)
    val invocationLocationOpt = invocationExprOpt.flatMap(_.locationOpt)
    function match {
      case MashString(memberName, _) ⇒
        callStringAsFunction(memberName, arguments, functionLocationOpt, invocationLocationOpt)
      case f: MashFunction ⇒
        Evaluator.addLocationToExceptionIfMissing(invocationLocationOpt) {
          f(arguments)
        }
      case BoundMethod(target, method, _) ⇒
        Evaluator.addLocationToExceptionIfMissing(invocationLocationOpt) {
          method(target, arguments)
        }
      case _ ⇒
        throw EvaluatorException(s"Not callable", functionLocationOpt.map(SourceLocation))
    }
  }

  private def callStringAsFunction(s: String, arguments: Arguments, functionLocationOpt: Option[PointedRegion], invocationLocationOpt: Option[PointedRegion]): MashValue =
    arguments.positionArgs match {
      case Seq(EvaluatedArgument.PositionArg(xs: MashList, _)) ⇒
        xs.map { target ⇒
          val intermediateResult = MemberEvaluator.lookup(target, s, functionLocationOpt)
          Evaluator.addLocationToExceptionIfMissing(invocationLocationOpt) {
            Evaluator.immediatelyResolveNullaryFunctions(intermediateResult)
          }
        }
      case Seq(EvaluatedArgument.PositionArg(target, _)) ⇒
        val intermediateResult = MemberEvaluator.lookup(target, s, functionLocationOpt)
        Evaluator.addLocationToExceptionIfMissing(invocationLocationOpt) {
          Evaluator.immediatelyResolveNullaryFunctions(intermediateResult)
        }
      case _ ⇒
        throw EvaluatorException(s"Cannot call a String on multiple arguments", invocationLocationOpt.map(SourceLocation))
    }

}