package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.QuotationType
import com.github.mdr.mash.runtime._

import scala.PartialFunction.condOpt

case class EvaluationContext(scopeStack: ScopeStack)

object Evaluator extends EvaluatorHelper {

  private val environmentInteractions = LinuxEnvironmentInteractions

  def evaluate(expr: Expr)(implicit context: EvaluationContext): MashValue = {
    try {
      ExecutionContext.checkInterrupted()
      val simpleResult = simpleEvaluate(expr)
      ExecutionContext.checkInterrupted()
      val finalResult = expr match {
        case _: Identifier | _: MemberExpr ⇒ immediatelyResolveNullaryFunctions(simpleResult, sourceLocation(expr))
        case _                             ⇒ simpleResult
      }
      finalResult
    } catch {
      case e: EvaluatorException             ⇒
        throw e
      case e: EvaluationInterruptedException ⇒
        throw e
      case t: Exception                      ⇒
        throw EvaluatorException("Unexpected error in evaluation: " + t.toString,
          stack = sourceLocation(expr).toList.map(loc ⇒ StackTraceItem(loc)),
          cause = t)
    }
  }

  /**
    * If the given value is a function or bound method that allows nullary invocation, invoke it immediately and
    * return the result.
    */
  def immediatelyResolveNullaryFunctions(v: MashValue, locationOpt: Option[SourceLocation]): MashValue =
    v match {
      case f: MashFunction if f.allowsNullary                     ⇒
        InvocationEvaluator.addInvocationToStackOnException(locationOpt, Some(f)) {
          f(Arguments())
        }
      case BoundMethod(target, method, _) if method.allowsNullary ⇒
        InvocationEvaluator.addInvocationToStackOnException(locationOpt) {
          method(target, Arguments())
        }
      case _                                                      ⇒ v
    }

  /**
    * Evaluate the given expression. If the result is a function/bound method that allows a nullary call, it is not called.
    */
  def simpleEvaluate(expr: Expr)(implicit context: EvaluationContext): MashValue =
    expr match {
      case Hole(_) | PipeExpr(_, _, _) | HeadlessMemberExpr(_, _, _) ⇒ // Should have been removed from the AST by now
        throw new EvaluatorException("Unexpected AST node: " + expr, sourceLocation(expr))
      case thisExpr: ThisExpr                                        ⇒ evaluateThisExpr(thisExpr)
      case interpolatedString: InterpolatedString                    ⇒ evaluateInterpolatedString(interpolatedString)
      case ParenExpr(body, _)                                        ⇒ evaluate(body)
      case blockExpr: BlockExpr                                      ⇒ evaluateBlockExpr(blockExpr)
      case Literal(v, _)                                             ⇒ v
      case memberExpr: MemberExpr                                    ⇒ MemberEvaluator.evaluateMemberExpr(memberExpr, immediatelyResolveNullaryWhenVectorising = true).result
      case lookupExpr: LookupExpr                                    ⇒ LookupEvaluator.evaluateLookupExpr(lookupExpr)
      case invocationExpr: InvocationExpr                            ⇒ InvocationEvaluator.evaluateInvocationExpr(invocationExpr)
      case LambdaExpr(params, body, _)                               ⇒ makeAnonymousFunction(params, body)
      case binOp: BinOpExpr                                          ⇒ BinaryOperatorEvaluator.evaluateBinOpExpr(binOp)
      case chainedOpExpr: ChainedOpExpr                              ⇒ BinaryOperatorEvaluator.evaluateChainedOp(chainedOpExpr)
      case assExpr: AssignmentExpr                                   ⇒ AssignmentEvaluator.evaluateAssignment(assExpr)
      case assExpr: PatternAssignmentExpr                            ⇒ AssignmentEvaluator.evaluatePatternAssignment(assExpr)
      case ifExpr: IfExpr                                            ⇒ evaluateIfExpr(ifExpr)
      case ListExpr(elements, _)                                     ⇒ MashList(elements.map(evaluate(_)))
      case mishExpr: MishExpr                                        ⇒ MishEvaluator.evaluateMishExpr(mishExpr)
      case expr: MishInterpolation                                   ⇒ MishEvaluator.evaluateMishInterpolation(expr)
      case MishFunction(command, _)                                  ⇒ SystemCommandFunction(command)
      case decl: FunctionDeclaration                                 ⇒ evaluateFunctionDecl(decl)
      case decl: ClassDeclaration                                    ⇒ evaluateClassDecl(decl)
      case helpExpr: HelpExpr                                        ⇒ HelpEvaluator.evaluateHelpExpr(helpExpr)
      case StatementSeq(statements, _)                               ⇒ evaluateStatements(statements)
      case lit: StringLiteral                                        ⇒ evaluateStringLiteral(lit)
      case MinusExpr(subExpr, _)                                     ⇒ evaluateMinusExpr(subExpr)
      case identifier: Identifier                                    ⇒ evaluateIdentifier(identifier)
      case objectExpr: ObjectExpr                                    ⇒ evaluateObjectExpr(objectExpr)
    }

  def evaluateObjectExpr(objectExpr: ObjectExpr)(implicit context: EvaluationContext): MashObject = {
    def getFieldName(fieldNameExpr: Expr): String =
      fieldNameExpr match {
        case Identifier(name, _) ⇒
          name
        case _                   ⇒
          evaluate(fieldNameExpr) match {
            case MashString(s, _) ⇒ s
            case x                ⇒ throw new EvaluatorException("Invalid object label of type " + x.typeName, sourceLocation(fieldNameExpr))
          }
      }
    val fields = objectExpr.fields.map {
      case FullObjectEntry(field, value, _)           ⇒ getFieldName(field) -> evaluate(value)
      case ShorthandObjectEntry(field, sourceInfoOpt) ⇒ field -> evaluateIdentifier(field, sourceInfoOpt.map(_.location))
    }
    MashObject.of(fields)
  }

  def evaluateBlockExpr(blockExpr: BlockExpr)(implicit context: EvaluationContext): MashValue = {
    val newContext = context.copy(scopeStack = context.scopeStack.withBlockScope(Seq()))
    Evaluator.evaluate(blockExpr.expr)(newContext)
  }

  def evaluateThisExpr(thisExpr: ThisExpr)(implicit context: EvaluationContext): MashValue = context.scopeStack.thisOpt.getOrElse {
    throw new EvaluatorException(s"No binding for 'this'", sourceLocation(thisExpr))
  }


  def evaluateIdentifier(identifier: Identifier)(implicit context: EvaluationContext): MashValue =
    evaluateIdentifier(identifier.name, sourceLocation(identifier))

  private def evaluateIdentifier(name: String, locationOpt: Option[SourceLocation])(implicit context: EvaluationContext): MashValue =
    context.scopeStack.lookup(name).getOrElse {
      throw new EvaluatorException(s"No binding for '$name'", locationOpt)
    }

  private def evaluateMinusExpr(subExpr: Expr)(implicit context: EvaluationContext): MashValue = evaluate(subExpr) match {
    case n: MashNumber ⇒ n.negate
    case x             ⇒ throw new EvaluatorException("Could not negate a value of type " + x.typeName, sourceLocation(subExpr))
  }

  def evaluateStringLiteral(lit: StringLiteral): MashValue = {
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
    val function = userDefinedFunction(decl)
    context.scopeStack.set(function.name, function)
    MashUnit
  }

  private def evaluateClassDecl(decl: ClassDeclaration)(implicit context: EvaluationContext): MashUnit = {
    val ClassDeclaration(className, paramList, bodyOpt, _) = decl
    val methodDecls = bodyOpt.map(_.methods).getOrElse(Seq())
    val classParams = parameterModel(paramList, Some(context))
    val classFields = classParams.params.map { param ⇒
      val fieldName = param.nameOpt.getOrElse("anon")
      Field(fieldName, s"Field '$fieldName'", AnyClass)
    }

    def makeMethod(decl: FunctionDeclaration)(implicit context: EvaluationContext): MashMethod = {
      val FunctionDeclaration(functionName, paramList, body, _) = decl
      val methodParams = parameterModel(paramList, Some(context))

      object Method extends MashMethod(functionName) {

        override def apply(target: MashValue, arguments: Arguments): MashValue = {
          val boundParams = params.validate(arguments)
          val newScopeStack = context.scopeStack.withFullScope(boundParams.boundNames, target)
          Evaluator.evaluate(body)(context.copy(scopeStack = newScopeStack))
        }

        override def summary: String = s"Method '$functionName'"

        override def params: ParameterModel = methodParams
      }
      Method
    }

    object UserDefinedClass extends MashClass(nameOpt = Some(className)) {

      override val fields = classFields

      override def summary: String = s"Class $className"

      override val staticMethods = Seq(
        NewStaticMethod)

      override val methods = methodDecls.map(makeMethod)

      object NewStaticMethod extends MashFunction("new") {
        override def apply(arguments: Arguments): MashObject = {
          val boundParams = params.validate(arguments)
          val fields =
            for (param <- params.params)
              yield param.nameOpt.getOrElse("anon") -> boundParams(param)
          MashObject.of(fields, UserDefinedClass)
        }

        override def summary: String = s"Construct a new $className"

        override def params: ParameterModel = classParams

        override def typeInferenceStrategy = UserDefinedClass

      }

    }
    context.scopeStack.set(className, UserDefinedClass)
    MashUnit
  }

  private def userDefinedFunction(decl: FunctionDeclaration)(implicit context: EvaluationContext): UserDefinedFunction = {
    val FunctionDeclaration(functionName, paramList, body, _) = decl
    val params = parameterModel(paramList, Some(context))
    new UserDefinedFunction(functionName, params, body, context)
  }

  private def makeParameter(param: FunctionParam, argIndex: Int, evaluationContextOpt: Option[EvaluationContext]): Parameter = {
    val FunctionParam(nameOpt, isVariadic, defaultExprOpt, isLazy, patternOpt, _) = param
    val defaultValueGeneratorOpt = evaluationContextOpt.flatMap(implicit context ⇒ defaultExprOpt.map(defaultExpr ⇒ () ⇒ evaluate(defaultExpr)))
    val summary = nameOpt.map(name ⇒ s"Parameter '$name'").getOrElse("Anonymous parameter")
    Parameter(nameOpt, summary, defaultValueGeneratorOpt = defaultValueGeneratorOpt,
      isVariadic = isVariadic, isLazy = isLazy, bindsName = nameOpt.isDefined, patternOpt = patternOpt.map(makeParamPattern))
  }

  private def makeParamEntry(entry: ObjectPatternEntry): ParamPattern.ObjectEntry =
    ParamPattern.ObjectEntry(entry.field, entry.valuePatternOpt map makeParamPattern)

  def makeParamPattern(pattern: Pattern): ParamPattern = pattern match {
    case ObjectPattern(entries, _)   ⇒ ParamPattern.Object(entries.map(makeParamEntry))
    case HolePattern(_)              ⇒ ParamPattern.Hole
    case IdentPattern(identifier, _) ⇒ ParamPattern.Ident(identifier)
    case ListPattern(patterns, _)    ⇒ ParamPattern.List(patterns.map(makeParamPattern))
  }

  def parameterModel(paramList: ParamList, evaluationContextOpt: Option[EvaluationContext] = None): ParameterModel = {
    val parameters: Seq[Parameter] = paramList.params.zipWithIndex.map { case (p, i) ⇒ makeParameter(p, i, evaluationContextOpt) }
    for (context <- evaluationContextOpt)
      verifyParameters(paramList)(context)
    ParameterModel(parameters)
  }

  private def verifyParameters(paramList: ParamList)(implicit context: EvaluationContext) {
    val params = paramList.params
    if (params.count(_.isVariadic) > 1)
      throw new EvaluatorException("Multiple variadic parameters are not allowed")
    val variadicIndex = params.indexWhere(_.isVariadic)
    if (variadicIndex >= 0 && variadicIndex < params.size - 1)
      throw new EvaluatorException("A variadic parameter must be the last positional parameter")
    for ((name, params) ← params.groupBy(_.nameOpt).collect { case (Some(name), ps) if ps.length > 1 ⇒ name -> ps }.headOption)
      throw new EvaluatorException(s"Duplicate parameter $name", params.lastOption.flatMap(sourceLocation))
  }

  private def evaluateInterpolatedString(interpolatedString: InterpolatedString)(implicit context: EvaluationContext): MashString = {
    val InterpolatedString(start, parts, end, _) = interpolatedString
    val chunks =
      MashString(start, PathClass) +:
        parts.map {
          case StringPart(s)  ⇒ MashString(s, PathClass)
          case ExprPart(expr) ⇒ evaluate(expr) match {
            case ms: MashString ⇒ ms
            case x              ⇒ MashString(ToStringifier.stringify(x))
          }
        } :+ MashString(end, PathClass)
    chunks.reduce(_ + _)
  }

  private def makeAnonymousFunction(paramList: ParamList, body: Expr)(implicit context: EvaluationContext): AnonymousFunction =
    AnonymousFunction(parameterModel(paramList, Some(context)), body, context)

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

}
