package com.github.mdr.mash.evaluator

import com.github.mdr.mash.classes.{ BoundMethod, UserDefinedClass, UserDefinedMethod }
import com.github.mdr.mash.compiler.DesugarHoles
import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.{ DocComment, QuotationType }
import com.github.mdr.mash.runtime._

import scala.PartialFunction.condOpt
import scala.util.control.NonFatal

object EvaluationContext {

  val NotUsed = EvaluationContext(ScopeStack(List()))

}

case class EvaluationContext(scopeStack: ScopeStack, namespaceOpt: Option[Namespace] = None)

object Evaluator extends EvaluatorHelper {

  private val environmentInteractions = LinuxEnvironmentInteractions

  def evaluate(expr: Expr)(implicit context: EvaluationContext): MashValue = {
    try {
      ExecutionContext.checkInterrupted()
      val simpleResult = simpleEvaluate(expr)
      ExecutionContext.checkInterrupted()
      val finalResult = expr match {
        case ident: Identifier ⇒
          if (context.scopeStack.lookup(ident.name).exists(_.isSafe))
            simpleResult
          else
            invokeNullaryFunctions(simpleResult, sourceLocation(expr))
        case _: MemberExpr     ⇒ invokeNullaryFunctions(simpleResult, sourceLocation(expr))
        case _                 ⇒ simpleResult
      }
      finalResult
    } catch {
      case e: EvaluatorException          ⇒
        throw e
      case EvaluationInterruptedException ⇒
        throw EvaluationInterruptedException
      case e: InterruptedException        ⇒
        throw EvaluationInterruptedException
      case NonFatal(t)                    ⇒
        throw EvaluatorException("Unexpected error in evaluation: " + t.toString,
          stack = sourceLocation(expr).toList.map(loc ⇒ StackTraceItem(Some(loc))),
          cause = t)
    }
  }

  /**
    * If the given value is a function or bound method that allows nullary invocation, invoke it immediately and
    * return the result; otherwise, return the input value unchanged.
    */
  def invokeNullaryFunctions(value: MashValue, locationOpt: Option[SourceLocation]): MashValue =
    value match {
      case f: MashFunction if f.allowsNullary                        ⇒
        InvocationEvaluator.addInvocationToStackOnException(locationOpt, Some(f)) {
          f.callNullary()
        }
      case bm@BoundMethod(target, method, _) if method.allowsNullary ⇒
        InvocationEvaluator.addInvocationToStackOnException(locationOpt, Some(bm)) {
          method.callNullary(target)
        }
      case _                                                         ⇒ value
    }

  /**
    * Evaluate the given expression. If the result is a function/bound method that allows a nullary call, it is not called.
    */
  def simpleEvaluate(expr: Expr)(implicit context: EvaluationContext): MashValue =
    expr match {
      case Hole(_, _) | PipeExpr(_, _, _) | HeadlessMemberExpr(_, _, _) ⇒ // Should have been removed from the AST by now
        throw new EvaluatorException("Unexpected AST node: " + expr, sourceLocation(expr))
      case thisExpr: ThisExpr                                           ⇒ evaluateThisExpr(thisExpr)
      case interpolatedString: InterpolatedString                       ⇒ evaluateInterpolatedString(interpolatedString)
      case ParenExpr(body, _)                                           ⇒ evaluate(body)
      case blockExpr: BlockExpr                                         ⇒ evaluateBlockExpr(blockExpr)
      case Literal(v, _)                                                ⇒ v
      case memberExpr: MemberExpr                                       ⇒ MemberEvaluator.evaluateMemberExpr(memberExpr, invokeNullaryWhenVectorising = true).result
      case lookupExpr: LookupExpr                                       ⇒ LookupEvaluator.evaluateLookupExpr(lookupExpr)
      case invocationExpr: InvocationExpr                               ⇒ InvocationEvaluator.evaluateInvocationExpr(invocationExpr)
      case LambdaExpr(params, body, _)                                  ⇒ makeAnonymousFunction(params, body)
      case binOp: BinOpExpr                                             ⇒ BinaryOperatorEvaluator.evaluateBinOpExpr(binOp)
      case chainedOpExpr: ChainedOpExpr                                 ⇒ BinaryOperatorEvaluator.evaluateChainedOp(chainedOpExpr)
      case assExpr: AssignmentExpr                                      ⇒ AssignmentEvaluator.evaluateAssignment(assExpr)
      case assExpr: PatternAssignmentExpr                               ⇒ AssignmentEvaluator.evaluatePatternAssignment(assExpr)
      case ifExpr: IfExpr                                               ⇒ evaluateIfExpr(ifExpr)
      case ListExpr(elements, _)                                        ⇒ MashList(elements.map(evaluate(_)))
      case mishExpr: MishExpr                                           ⇒ MishEvaluator.evaluateMishExpr(mishExpr)
      case expr: MishInterpolation                                      ⇒ MishEvaluator.evaluateMishInterpolation(expr)
      case MishFunction(command, _)                                     ⇒ SystemCommandFunction(command)
      case decl: FunctionDeclaration                                    ⇒ evaluateFunctionDecl(decl)
      case decl: ClassDeclaration                                       ⇒ evaluateClassDecl(decl)
      case helpExpr: HelpExpr                                           ⇒ HelpEvaluator.evaluateHelpExpr(helpExpr)
      case StatementSeq(statements, _)                                  ⇒ evaluateStatements(statements)
      case lit: StringLiteral                                           ⇒ evaluateStringLiteral(lit)
      case MinusExpr(subExpr, _)                                        ⇒ evaluateMinusExpr(subExpr)
      case identifier: Identifier                                       ⇒ evaluateIdentifier(identifier)
      case objectExpr: ObjectExpr                                       ⇒ evaluateObjectExpr(objectExpr)
      case importStatement: ImportStatement                             ⇒ evaluateImportStatement(importStatement)
    }

  private def evaluateImportStatement(importStatement: ImportStatement)(implicit context: EvaluationContext): MashValue = {
    val target = Evaluator.evaluate(importStatement.expr)
    importStatement.importNameOpt match {
      case Some(name) ⇒
        MemberEvaluator.maybeLookupByString(target, name) match {
          case Some(value) ⇒
            context.scopeStack.set(name, value)
            value
          case None        ⇒
            MemberEvaluator.throwCannotFindMemberException(target, name, sourceLocation(importStatement))
        }
      case None       ⇒
        for {
          name ← MemberEvaluator.getMemberNames(target)
          value ← MemberEvaluator.maybeLookupByString(target, name, includeShyMembers = false)
        } context.scopeStack.set(name, value)
        MashUnit
    }
  }

  def evaluateObjectExpr(objectExpr: ObjectExpr)(implicit context: EvaluationContext): MashObject = {
    def getFieldName(fieldNameExpr: Expr): MashValue =
      fieldNameExpr match {
        case Identifier(name, _) if !name.startsWith(DesugarHoles.VariableNamePrefix) ⇒
          MashString(name)
        case _                                                                        ⇒
          evaluate(fieldNameExpr)
      }
    val fields = objectExpr.fields.map {
      case FullObjectEntry(field, value, _)                 ⇒ getFieldName(field) -> evaluate(value)
      case entry@ShorthandObjectEntry(field, sourceInfoOpt) ⇒
        val evaluatedIdentifier = evaluateIdentifier(field, sourceInfoOpt.flatMap(_.locationOpt))
        val finalResult = invokeNullaryFunctions(evaluatedIdentifier, sourceLocation(entry))
        MashString(field) -> finalResult
    }
    MashObject.of(fields)
  }

  def evaluateBlockExpr(blockExpr: BlockExpr)(implicit context: EvaluationContext): MashValue = {
    val newContext = context.copy(scopeStack = context.scopeStack.withLeakyScope())
    Evaluator.evaluate(blockExpr.expr)(newContext)
  }

  def evaluateThisExpr(thisExpr: ThisExpr)(implicit context: EvaluationContext): MashValue =
    context.scopeStack.thisOpt.getOrElse {
      throw new EvaluatorException(s"No binding for 'this'", sourceLocation(thisExpr))
    }

  def evaluateIdentifier(identifier: Identifier)(implicit context: EvaluationContext): MashValue =
    evaluateIdentifier(identifier.name, sourceLocation(identifier))

  private def evaluateIdentifier(name: String,
                                 locationOpt: Option[SourceLocation])(implicit context: EvaluationContext): MashValue =
    context.scopeStack.lookup(name).getOrElse {
      val names = context.scopeStack.bindings.keys.toSeq
      throw new EvaluatorException(s"No binding for '$name'${Suggestor.suggestionSuffix(names, name)}", locationOpt)
    }.value

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

  private def evaluateFunctionDecl(decl: FunctionDeclaration)(implicit context: EvaluationContext): UserDefinedFunction = {
    val function = userDefinedFunction(decl)
    context.scopeStack.set(function.name, function)
    function
  }

  private def userDefinedFunction(decl: FunctionDeclaration)(implicit context: EvaluationContext): UserDefinedFunction = {
    val FunctionDeclaration(docCommentOpt, attributes, functionName, paramList, body, _) = decl
    evaluateAttributes(attributes)
    val params = parameterModel(paramList, Some(context), docCommentOpt)
    UserDefinedFunction(docCommentOpt, functionName, params, body, context, decl)
  }

  private def evaluateClassDecl(decl: ClassDeclaration)(implicit context: EvaluationContext): UserDefinedClass = {
    val ClassDeclaration(docCommentOpt, _, className, paramList, bodyOpt, _) = decl
    val params = parameterModel(paramList, Some(context), docCommentOpt)

    def makeMethod(decl: FunctionDeclaration)(implicit context: EvaluationContext): UserDefinedMethod = {
      val FunctionDeclaration(docCommentOpt, attributes, functionName, paramList, body, _) = decl
      val evaluatedAttributes = evaluateAttributes(attributes)
      val aliases = getAliases(evaluatedAttributes).distinct
      val isPrivate = evaluatedAttributes.exists(_.name == Attributes.Private)
      val methodParams = parameterModel(paramList, Some(context), docCommentOpt)
      UserDefinedMethod(docCommentOpt, functionName, methodParams, paramList, body, context, isPrivate, aliases, decl)
    }

    val methods = bodyOpt.map(_.methods).getOrElse(Seq()).map(makeMethod)

    val klass = UserDefinedClass(docCommentOpt, className, context.namespaceOpt, params, methods)
    context.scopeStack.set(className, klass)
    klass
  }

  private def getAliases(evaluatedAttributes: Seq[EvaluatedAttribute])(implicit context: EvaluationContext): Seq[String] = {
    for {
      attribute ← evaluatedAttributes.filter(_.name == Attributes.Alias)
      arguments = attribute.argumentsOpt getOrElse Seq()
      boundParams = AliasParameterModel.params.bindTo(Arguments(arguments), context)
      alias = boundParams.validateString(AliasParameterModel.Params.Name).s
    } yield alias
  }

  protected case class EvaluatedAttribute(name: String, argumentsOpt: Option[Seq[EvaluatedArgument[SuspendedMashValue]]])

  protected def evaluateAttributes(attributes: Seq[Attribute])(implicit context: EvaluationContext): Seq[EvaluatedAttribute] =
    attributes.map {
      attribute ⇒
        val argumentsOpt = attribute.argumentsOpt.map(arguments ⇒ arguments.map(InvocationEvaluator.evaluateArgument))
        EvaluatedAttribute(attribute.name, argumentsOpt)
    }

  private def getShortFlag(evaluatedAttributes: Seq[EvaluatedAttribute])(implicit context: EvaluationContext): Option[Char] = {
    val ShortFlag = Parameter(Some("shortName"))
    val params = ParameterModel(ShortFlag)
    for {
      attribute ← evaluatedAttributes.find(_.name == Attributes.ShortFlag)
      arguments = attribute.argumentsOpt getOrElse Seq()
      boundParams = params.bindTo(Arguments(arguments), context)
      name = boundParams.validateString(ShortFlag).s
    } yield
      if (name.length == 1) name.head
      else boundParams.throwInvalidArgument(ShortFlag, s"Short flag must be a single character, but was '$name'")
  }

  def makeParameter(param: FunctionParam,
                    evaluationContextOpt: Option[EvaluationContext] = None,
                    docCommentOpt: Option[DocComment] = None): Parameter = {
    val FunctionParam(attributes, nameOpt, isVariadic, defaultExprOpt, patternOpt, _) = param
    val shortFlagOpt = evaluationContextOpt.flatMap { implicit evaluationContextOpt ⇒
      getShortFlag(evaluateAttributes(attributes))
    }
    val isLazy = attributes.exists(_.name == Attributes.Lazy)
    val isFlag = attributes.exists(_.name == Attributes.Flag)
    val variadicAtLeastOne = attributes.exists(_.name == Attributes.AtLeastOne)
    val variadicFlatten = attributes.exists(_.name == Attributes.Flatten)
    val isNamedArgsParam = attributes.exists(_.name == Attributes.NamedArgs)
    val isSafe = attributes.exists(_.name == Attributes.Safe)

    val defaultValueGeneratorOpt: Option[ValueGenerator] =
      defaultExprOpt.map(defaultExpr ⇒ (context: EvaluationContext) ⇒ evaluate(defaultExpr)(context))
    val docSummaryOpt =
      for {
        name ← nameOpt
        docComment ← docCommentOpt
        paramComment ← docComment.getParamComment(name)
      } yield paramComment.summary

    Parameter(nameOpt, docSummaryOpt, defaultValueGeneratorOpt = defaultValueGeneratorOpt, shortFlagOpt = shortFlagOpt,
      isVariadic = isVariadic, isFlag = isFlag, isLazy = isLazy, isNamedArgsParam = isNamedArgsParam,
      variadicAtLeastOne = variadicAtLeastOne, variadicFlatten = variadicFlatten, isSafe = isSafe,
      patternOpt = patternOpt.map(makeParamPattern))
  }

  private def makeParamEntry(entry: ObjectPatternEntry): ParamPattern.ObjectEntry =
    ParamPattern.ObjectEntry(entry.field, entry.valuePatternOpt map makeParamPattern)

  def makeParamPattern(pattern: Pattern): ParamPattern = pattern match {
    case ObjectPattern(entries, _)   ⇒ ParamPattern.Object(entries.map(makeParamEntry))
    case HolePattern(_)              ⇒ ParamPattern.Hole
    case IdentPattern(identifier, _) ⇒ ParamPattern.Ident(identifier)
    case ListPattern(patterns, _)    ⇒ ParamPattern.List(patterns.map(makeParamPattern))
  }

  def parameterModel(paramList: ParamList,
                     evaluationContextOpt: Option[EvaluationContext] = None,
                     docCommentOpt: Option[DocComment] = None): ParameterModel = {
    val evaluationContext = evaluationContextOpt.getOrElse(EvaluationContext(ScopeStack(Nil)))
    val parameters: Seq[Parameter] = paramList.params.map(makeParameter(_, Some(evaluationContext), docCommentOpt))
    for (context ← evaluationContextOpt)
      verifyParameters(paramList)(context)
    ParameterModel(parameters)
  }

  private def verifyParameters(paramList: ParamList)(implicit context: EvaluationContext) {
    val params = paramList.params
    if (params.count(_.isVariadic) > 1)
      throw new EvaluatorException("Multiple variadic parameters are not allowed")
    for ((name, params) ← params.groupBy(_.nameOpt).collect {
      case (Some(name), ps) if ps.length > 1 ⇒ name -> ps
    }.headOption)
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

  private def makeAnonymousFunction(paramList: ParamList, body: Expr)(implicit context: EvaluationContext) =
    AnonymousFunction(parameterModel(paramList, Some(context)), body, context)

  private def evaluateIfExpr(ifExpr: IfExpr)(implicit context: EvaluationContext) = {
    val IfExpr(cond, body, elseOpt, _) = ifExpr
    val result = evaluate(cond)
    if (result.isTruthy)
      evaluate(body)
    else
      elseOpt.map(evaluate).getOrElse(MashUnit)
  }

}
