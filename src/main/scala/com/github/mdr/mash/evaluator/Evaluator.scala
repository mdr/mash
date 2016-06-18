package com.github.mdr.mash.evaluator

import java.time.Duration
import java.time.Instant
import java.time.temporal.TemporalAmount

import scala.PartialFunction.condOpt
import scala.collection.immutable.ListMap

import com.github.mdr.mash.functions.AnonymousFunction
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.UserDefinedFunction
import com.github.mdr.mash.ns.core.help.HelpFunction
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.ns.os.ProcessResultClass
import com.github.mdr.mash.ns.time.ChronoUnitClass
import com.github.mdr.mash.ns.time.MillisecondsClass
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.BinaryOperator
import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.parser.QuotationType
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.subprocesses.ProcessResult
import com.github.mdr.mash.subprocesses.ProcessRunner
import com.github.mdr.mash.utils.PointedRegion
import com.github.mdr.mash.utils.Utils

case class EvaluationContext(scopeStack: ScopeStack)

object Evaluator {

  private val environmentInteractions = LinuxEnvironmentInteractions

  def evaluate(expr: Expr)(implicit context: EvaluationContext): MashValue = {
    try {
      ExecutionContext.checkInterrupted()
      val v = simpleEvaluate(expr)
      ExecutionContext.checkInterrupted()
      val result = expr match {
        case _: Identifier | _: MemberExpr ⇒
          addLocationToExceptionIfMissing(expr.locationOpt) { immediatelyResolveNullaryFunctions(v) }
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
          locationOpt = expr.locationOpt,
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
        throw EvaluatorException("Unexpected AST node: " + expr, expr.locationOpt)
      case StringLiteral(s, quotationType, tildePrefix, _) ⇒
        val tagOpt = condOpt(quotationType) { case QuotationType.Double ⇒ PathClass }
        val detilded = if (tildePrefix) environmentInteractions.home + s else s
        MashString(detilded, tagOpt)
      case Identifier(name, _) ⇒
        context.scopeStack.lookup(name).getOrElse {
          throw EvaluatorException(s"No binding for '$name'", expr.locationOpt)
        }
      case MinusExpr(subExpr, _) ⇒
        evaluate(subExpr) match {
          case n: MashNumber ⇒ n.negate
          case _             ⇒ throw new EvaluatorException("Could not negate a non-number", expr.locationOpt)
        }
      case interpolatedString: InterpolatedString ⇒ evaluateInterpolatedString(interpolatedString)
      case ParenExpr(body, _)                     ⇒ evaluate(body)
      case Literal(v, _)                          ⇒ v
      case memberExpr: MemberExpr                 ⇒ MemberEvaluator.evaluateMemberExpr(memberExpr, immediatelyResolveNullaryWhenVectorising = true).result
      case lookupExpr: LookupExpr                 ⇒ evaluateLookupExpr(lookupExpr)
      case invocationExpr: InvocationExpr         ⇒ InvocationEvaluator.evaluateInvocationExpr(invocationExpr)
      case LambdaExpr(parameter, body, _)         ⇒ makeAnonymousFunction(parameter, body)
      case binOp: BinOpExpr                       ⇒ BinaryOperatorEvaluator.evaluateBinOp(binOp)
      case chainedOpExpr: ChainedOpExpr           ⇒ BinaryOperatorEvaluator.evaluateChainedOp(chainedOpExpr)
      case assExpr: AssignmentExpr                ⇒ AssignmentEvaluator.evaluateAssignment(assExpr)
      case ifExpr: IfExpr                         ⇒ evaluateIfExpr(ifExpr)
      case ListExpr(items, _)                     ⇒ MashList(items.map(evaluate(_)))
      case mishExpr: MishExpr                     ⇒ evaluateMishExpr(mishExpr)
      case expr: MishInterpolation                ⇒ evaluateMishInterpolation(expr)
      case decl: FunctionDeclaration              ⇒ evaluateFunctionDecl(decl)
      case MishFunction(command, _)               ⇒ SystemCommandFunction(command)
      case HelpExpr(expr, _)                      ⇒ evaluateHelpExpr(expr)
      case ObjectExpr(entries, _) ⇒
        val fields = for ((label, value) ← entries) yield label -> evaluate(value)
        MashObject(fields, classOpt = None)
      case StatementSeq(statements, _) ⇒
        var result: MashValue = MashUnit
        for (statement ← statements)
          result = evaluate(statement)
        result
    }

  private def evaluateFunctionDecl(decl: FunctionDeclaration)(implicit context: EvaluationContext): MashUnit = {
    val FunctionDeclaration(name, params, body, sourceInfoOpt) = decl
    val parameters: Seq[Parameter] = params.map(param ⇒
      Parameter(name, s"Parameter '$name'", isVariadic = param.isVariadic))
    if (parameters.count(_.isVariadic) > 1)
      throw new EvaluatorException("Multiple variadic parameters are not allowed")
    val variadicIndex = parameters.indexWhere(_.isVariadic)
    if (variadicIndex >= 0 && variadicIndex < params.size - 1)
      throw new EvaluatorException("A variadic parameter must be the last positional parameter")
    val fn = new UserDefinedFunction(name, ParameterModel(parameters), body, context)
    context.scopeStack.set(name, fn)
    MashUnit
  }
  private def lookupField(target: MashValue, name: String): Option[(Field, MashClass)] =
    condOpt(target) {
      case MashObject(_, Some(klass)) ⇒ klass.fields.find(_.name == name).map(field ⇒ (field, klass))
    }.flatten

  private def getHelpForMember(target: MashValue, name: String): Option[MashObject] = {
    val fieldHelpOpt = lookupField(target, name).map { case (field, klass) ⇒ HelpFunction.getHelp(field, klass) }
    lazy val memberHelpOpt = MemberEvaluator.maybeLookup(target, name).collect {
      case method: BoundMethod ⇒ HelpFunction.getHelp(method)
    }
    fieldHelpOpt orElse memberHelpOpt
  }

  private def evaluateHelpExpr(expr: Expr)(implicit context: EvaluationContext): MashObject =
    expr match {
      case memberExpr @ MemberExpr(targetExpr, name, _, _) ⇒
        val target = evaluate(targetExpr)
        val scalarHelpOpt = getHelpForMember(target, name)
        lazy val vectorHelpOpt = condOpt(target) {
          case MashList(x, _*) ⇒ getHelpForMember(x, name)
        }.flatten
        lazy val directHelp = {
          val result = MemberEvaluator.evaluateMemberExpr_(memberExpr, target, context, immediatelyResolveNullaryWhenVectorising = true).result
          HelpFunction.getHelp(result)
        }
        scalarHelpOpt orElse vectorHelpOpt getOrElse directHelp
      case _ ⇒
        val x = simpleEvaluate(expr)
        HelpFunction.getHelp(x)
    }

  private def evaluateMishInterpolation(expr: MishInterpolation)(implicit context: EvaluationContext) =
    expr.part match {
      case StringPart(s)  ⇒ MashString(s, PathClass)
      case ExprPart(expr) ⇒ evaluate(expr)
    }

  private def evaluateMishExpr(expr: MishExpr)(implicit context: EvaluationContext): MashValue = {
    val MishExpr(command, args, captureProcessOutput, _) = expr
    val evaluatedCommand = evaluate(command)
    val evaluatedArgs = args.map(evaluate(_))
    val flattenedArgs: Seq[MashValue] = evaluatedArgs.flatMap {
      case xs: MashList ⇒ xs.items
      case x            ⇒ Seq(x)
    }
    val allArgs = evaluatedCommand +: flattenedArgs
    if (captureProcessOutput) {
      val processResult = ProcessRunner.runProcess(allArgs, expandTilde = true, captureProcess = captureProcessOutput)
      ProcessResultClass.fromResult(processResult)
    } else {
      ProcessRunner.runProcess(allArgs, expandTilde = true)
      MashUnit
    }
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
    if (Truthiness.isTruthy(result))
      evaluate(body)
    else elseOpt match {
      case None           ⇒ MashUnit
      case Some(elseBody) ⇒ evaluate(elseBody)
    }
  }

  private def evaluateLookupExpr(lookupExpr: LookupExpr)(implicit context: EvaluationContext): MashValue = {
    val LookupExpr(targetExpr, indexExpr, _) = lookupExpr
    val target = evaluate(targetExpr)
    val index = evaluate(indexExpr)
    val targetLocationOpt = targetExpr.locationOpt
    val indexLocationOpt = indexExpr.locationOpt
    val lookupLocationOpt = lookupExpr.locationOpt
    index match {
      case MashString(memberName, _) ⇒ MemberEvaluator.lookup(target, memberName, indexLocationOpt)
      case n: MashNumber ⇒
        val i = n.asInt.getOrElse(throw new EvaluatorException("Unable to lookup, non-integer index: " + n, lookupLocationOpt))
        target match {
          case xs: MashList ⇒
            val index = if (i < 0) i + xs.size else i
            if (index >= xs.size)
              throw new EvaluatorException("Index out of range " + n, indexLocationOpt)
            xs(index)
          case s: MashString ⇒ s.lookup(i)
          case _             ⇒ throw new EvaluatorException("Unable to lookup", lookupLocationOpt)
        }
      case _ ⇒
        throw new EvaluatorException("Unable to lookup", indexLocationOpt)
    }
  }

  def addLocationToExceptionIfMissing[T <: MashValue](locationOpt: Option[PointedRegion])(p: ⇒ T): T =
    try
      p
    catch {
      case e: EvaluatorException if e.locationOpt.isEmpty ⇒
        throw e.copy(locationOpt = locationOpt)
    }
}
