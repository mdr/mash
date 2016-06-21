package com.github.mdr.mash.evaluator

import scala.PartialFunction.condOpt

import com.github.mdr.mash.functions.AnonymousFunction
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.UserDefinedFunction
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.QuotationType
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.utils.PointedRegion

case class EvaluationContext(scopeStack: ScopeStack)

object Evaluator extends EvaluatorHelper {

  private val environmentInteractions = LinuxEnvironmentInteractions

  def evaluate(expr: Expr)(implicit context: EvaluationContext): MashValue = {
    try {
      ExecutionContext.checkInterrupted()
      val v = simpleEvaluate(expr)
      ExecutionContext.checkInterrupted()
      val result = expr match {
        case _: Identifier | _: MemberExpr ⇒
          addLocationToExceptionIfMissing(sourceLocation(expr)) { immediatelyResolveNullaryFunctions(v) }
        case _ ⇒ v
      }
      result
    } catch {
      case e: EvaluatorException ⇒
        throw e
      case e: EvaluationInterruptedException ⇒
        throw e
      case t: Exception ⇒
        throw EvaluatorException("Unexpected error in evaluation: " + t.toString,
          locationOpt = sourceLocation(expr),
          cause = t)
    }
  }

  /**
   * If the given value is a function or bound method that allows nullary invocation, invoke it immediately and
   * return the result.
   */
  def immediatelyResolveNullaryFunctions(v: MashValue): MashValue =
    v match {
      case f: MashFunction if f.allowsNullary ⇒ f(Arguments(Seq()))
      case BoundMethod(target, method, _) if method.allowsNullary ⇒ method(target, Arguments(Seq()))
      case _ ⇒ v
    }

  /**
   * Evaluate the given expression. If the result is a function/bound method that allows a nullary call, it is not called.
   */
  def simpleEvaluate(expr: Expr)(implicit context: EvaluationContext): MashValue =
    expr match {
      case Hole(_) | PipeExpr(_, _, _) | HeadlessMemberExpr(_, _, _) ⇒ // Should have been removed from the AST by now
        throw EvaluatorException("Unexpected AST node: " + expr, sourceLocation(expr))
      case interpolatedString: InterpolatedString ⇒ evaluateInterpolatedString(interpolatedString)
      case ParenExpr(body, _)                     ⇒ evaluate(body)
      case Literal(v, _)                          ⇒ v
      case memberExpr: MemberExpr                 ⇒ MemberEvaluator.evaluateMemberExpr(memberExpr, immediatelyResolveNullaryWhenVectorising = true).result
      case lookupExpr: LookupExpr                 ⇒ LookupEvaluator.evaluateLookupExpr(lookupExpr)
      case invocationExpr: InvocationExpr         ⇒ InvocationEvaluator.evaluateInvocationExpr(invocationExpr)
      case LambdaExpr(parameter, body, _)         ⇒ makeAnonymousFunction(parameter, body)
      case binOp: BinOpExpr                       ⇒ BinaryOperatorEvaluator.evaluateBinOp(binOp)
      case chainedOpExpr: ChainedOpExpr           ⇒ BinaryOperatorEvaluator.evaluateChainedOp(chainedOpExpr)
      case assExpr: AssignmentExpr                ⇒ AssignmentEvaluator.evaluateAssignment(assExpr)
      case ifExpr: IfExpr                         ⇒ evaluateIfExpr(ifExpr)
      case ListExpr(items, _)                     ⇒ MashList(items.map(evaluate(_)))
      case mishExpr: MishExpr                     ⇒ MishEvaluator.evaluateMishExpr(mishExpr)
      case expr: MishInterpolation                ⇒ MishEvaluator.evaluateMishInterpolation(expr)
      case MishFunction(command, _)               ⇒ SystemCommandFunction(command)
      case decl: FunctionDeclaration              ⇒ evaluateFunctionDecl(decl)
      case HelpExpr(expr, _)                      ⇒ HelpEvaluator.evaluateHelpExpr(expr)
      case StatementSeq(statements, _)            ⇒ evaluateStatements(statements)
      case lit: StringLiteral                     ⇒ evaluateStringLiteral(lit)
      case MinusExpr(subExpr, _)                  ⇒ evaluateMinusExpr(subExpr)
      case Identifier(name, _) ⇒
        context.scopeStack.lookup(name).getOrElse {
          throw EvaluatorException(s"No binding for '$name'", sourceLocation(expr))
        }
      case ObjectExpr(entries, _) ⇒
        val fields = for ((label, value) ← entries) yield label -> evaluate(value)
        MashObject(fields, classOpt = None)
    }

  private def evaluateMinusExpr(subExpr: Expr)(implicit context: EvaluationContext): MashValue = evaluate(subExpr) match {
    case n: MashNumber ⇒ n.negate
    case x             ⇒ throw new EvaluatorException("Could not negate a value of type " + x.primaryClass, sourceLocation(subExpr))
  }

  private def evaluateStringLiteral(lit: StringLiteral): MashValue = {
    val StringLiteral(s, quotationType, tildePrefix, _) = lit
    val tagOpt = condOpt(quotationType) { case QuotationType.Double ⇒ PathClass }
    val detilded = if (tildePrefix) environmentInteractions.home + s else s
    MashString(detilded, tagOpt)
  }

  private def evaluateStatements(statements: Seq[Expr])(implicit context: EvaluationContext): MashValue = {
    var result: MashValue = MashUnit
    for (statement ← statements)
      result = evaluate(statement)
    result
  }

  private def evaluateFunctionDecl(decl: FunctionDeclaration)(implicit context: EvaluationContext): MashUnit = {
    val FunctionDeclaration(functionName, params, body, sourceInfoOpt) = decl
    val parameters: Seq[Parameter] = params.map(param ⇒
      Parameter(param.name, s"Parameter '${param.name}'", isVariadic = param.isVariadic))
    if (parameters.count(_.isVariadic) > 1)
      throw new EvaluatorException("Multiple variadic parameters are not allowed")
    val variadicIndex = parameters.indexWhere(_.isVariadic)
    if (variadicIndex >= 0 && variadicIndex < params.size - 1)
      throw new EvaluatorException("A variadic parameter must be the last positional parameter")
    val fn = new UserDefinedFunction(functionName, ParameterModel(parameters), body, context)
    context.scopeStack.set(functionName, fn)
    MashUnit
  }

  private def evaluateInterpolatedString(interpolatedString: InterpolatedString)(implicit context: EvaluationContext): MashString = {
    val InterpolatedString(start, parts, end, _) = interpolatedString
    val chunks =
      MashString(start, PathClass) +:
        parts.map {
          case StringPart(s) ⇒ MashString(s, PathClass)
          case ExprPart(expr) ⇒ evaluate(expr) match {
            case ms: MashString ⇒ ms
            case x              ⇒ MashString(ToStringifier.stringify(x))
          }
        } :+ MashString(end, PathClass)
    chunks.reduce(_ + _)
  }

  private def makeAnonymousFunction(parameter: String, body: Expr)(implicit context: EvaluationContext): AnonymousFunction =
    AnonymousFunction(parameter, body, context)

  private def evaluateIfExpr(ifExpr: IfExpr)(implicit context: EvaluationContext) = {
    val IfExpr(cond, body, elseOpt, _) = ifExpr
    val result = evaluate(cond)
    if (result.isTruthy)
      evaluate(body)
    else elseOpt match {
      case None           ⇒ MashUnit
      case Some(elseBody) ⇒ evaluate(elseBody)
    }
  }

  def addLocationToExceptionIfMissing[T <: MashValue](locationOpt: Option[SourceLocation])(p: ⇒ T): T =
    try
      p
    catch {
      case e: EvaluatorException if e.locationOpt.isEmpty ⇒
        throw e.copy(locationOpt = locationOpt)
    }

}
