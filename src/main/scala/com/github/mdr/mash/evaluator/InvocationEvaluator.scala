package com.github.mdr.mash.evaluator

import com.github.mdr.mash.evaluator.MemberEvaluator.MemberExprEvalResult
import com.github.mdr.mash.functions.{ ArgumentException, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime._

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
      val suspendedValue = SuspendedMashValue(() ⇒ Evaluator.evaluate(expr))
      EvaluatedArgument.PositionArg(suspendedValue, Some(arg))
    case Argument.ShortFlag(flags, sourceInfoOpt) ⇒
      EvaluatedArgument.ShortFlag(flags, Some(arg))
    case Argument.LongFlag(flag, valueOpt, sourceInfoOpt) ⇒
      val suspendedValueOpt = valueOpt.map { expr ⇒
        SuspendedMashValue(() ⇒ Evaluator.evaluate(expr))
      }
      EvaluatedArgument.LongFlag(flag, suspendedValueOpt, Some(arg))
  }

  private def callFunction(function: MashValue, arguments: Arguments, functionExpr: Expr, invocationExpr: Expr)(implicit context: EvaluationContext): MashValue =
    callFunction(function, arguments, sourceLocation(functionExpr), sourceLocation(invocationExpr))

  def callFunction(function: MashValue,
                   arguments: Arguments,
                   functionLocationOpt: Option[SourceLocation] = None,
                   invocationLocationOpt: Option[SourceLocation] = None): MashValue =
    function match {
      case MashString(memberName, _) ⇒
        val f = new StringFunction(memberName, functionLocationOpt, invocationLocationOpt)
        addInvocationToStackOnException(invocationLocationOpt, Some(f)) {
          f(arguments)
        }
      case b: MashBoolean ⇒
        val f = new BooleanFunction(b.value)
        addInvocationToStackOnException(invocationLocationOpt, Some(f)) {
          f(arguments)
        }
      case f: MashFunction ⇒
        addInvocationToStackOnException(invocationLocationOpt, Some(f)) {
          f(arguments)
        }
      case BoundMethod(target, method, _) ⇒
        addInvocationToStackOnException(invocationLocationOpt, None) {
          method(target, arguments)
        }
      case klass: MashClass ⇒
        klass.getStaticMethod("new") match {
          case Some(staticMethod) ⇒ callFunction(staticMethod, arguments, functionLocationOpt, invocationLocationOpt)
          case None ⇒ throw new EvaluatorException(s"Value of type ${klass.typeName} is not callable", functionLocationOpt)
        }
      case x ⇒
        throw new EvaluatorException(s"Value of type ${x.typeName} is not callable", functionLocationOpt)
    }

  private class BooleanFunction(b: Boolean) extends MashFunction() {

    object Params {
      val Then = Parameter(
        nameOpt = Some("then"),
        summary = "The result if this is true",
        isLazy = true)
      val Else = Parameter(
        nameOpt = Some("else"),
        summary = "The result if this is false",
        defaultValueGeneratorOpt = Some(() ⇒ MashUnit),
        isLazy = true)
    }
    import Params._

    val params = ParameterModel(Seq(Then, Else))

    def apply(arguments: Arguments): MashValue = {
      val boundParams = params.validate(arguments)
      if (b)
        boundParams(Then).asInstanceOf[MashFunction].apply(Arguments(Seq()))
      else
        boundParams(Else) match {
          case MashUnit        ⇒ MashUnit
          case f: MashFunction ⇒ f.apply(Arguments(Seq()))
        }
    }

    override def summary = "Boolean as a function"

  }

  private class StringFunction(s: String,
                               functionLocationOpt: Option[SourceLocation],
                               invocationLocationOpt: Option[SourceLocation]) extends MashFunction() {

    object Params {
      val Target = Parameter(
        nameOpt = Some("target"),
        summary = "Member to look-up in the target object")
    }
    import Params._

    val params = ParameterModel(Seq(Target))

    def apply(arguments: Arguments): MashValue = {
      val boundParams = params.validate(arguments)
      boundParams(Target) match {
        case xs: MashList ⇒
          xs.map { target ⇒
            val intermediateResult = MemberEvaluator.lookup(target, s, functionLocationOpt)
            Evaluator.immediatelyResolveNullaryFunctions(intermediateResult, invocationLocationOpt)
          }
        case v ⇒
          val intermediateResult = MemberEvaluator.lookup(v, s, functionLocationOpt)
          Evaluator.immediatelyResolveNullaryFunctions(intermediateResult, functionLocationOpt)
      }
    }

    override def summary = "String as a function"

  }

  def addInvocationToStackOnException[T](invocationLocationOpt: Option[SourceLocation], functionOpt: Option[MashFunction] = None)(p: ⇒ T): T =
    try
      p
    catch {
      case e: ArgumentException ⇒
        throw new EvaluatorException(e.message, e.locationOpt orElse invocationLocationOpt)
      case e: EvaluatorException ⇒
        throw invocationLocationOpt.map(e.lastWasFunction(functionOpt).push).getOrElse(e)
    }

}