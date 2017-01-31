package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.{ Token, TokenType }
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.AbstractSyntax.Argument
import com.github.mdr.mash.runtime.{ MashBoolean, MashNull, MashNumber }

import scala.PartialFunction.condOpt

/**
  * Convert from concrete to abstract syntax trees.
  */
class Abstractifier(provenance: Provenance) {

  import com.github.mdr.mash.parser.{ AbstractSyntax ⇒ Abstract, ConcreteSyntax ⇒ Concrete }

  def abstractify(program: Concrete.Program): Abstract.Program = {
    val namespaceOpt = program.namespaceOpt.map(abstractifyNamespace(_))
    Abstract.Program(namespaceOpt, abstractify(program.body), sourceInfo(program))
  }

  private def abstractifyNamespace(namespace: Concrete.NamespaceDeclaration): Abstract.NamespaceDeclaration = {
    val segments = namespace.firstSegment.text +: namespace.dotSegments.map(_._2.text)
    Abstract.NamespaceDeclaration(segments, sourceInfo(namespace))
  }

  private def abstractify(expr: Concrete.Expr): Abstract.Expr = expr match {
    case Concrete.Literal(token)                            ⇒ abstractifyLiteral(token, sourceInfo(expr))
    case Concrete.Identifier(token)                         ⇒ Abstract.Identifier(token.text, sourceInfo(expr))
    case Concrete.Hole(_)                                   ⇒ Abstract.Hole(sourceInfo(expr))
    case Concrete.PipeExpr(left, _, right)                  ⇒ Abstract.PipeExpr(abstractify(left), abstractify(right), sourceInfo(expr))
    case memberExpr@Concrete.MemberExpr(e, _, name)         ⇒ Abstract.MemberExpr(abstractify(e), name.text, memberExpr.isNullSafe, sourceInfo(expr))
    case memberExpr@Concrete.HeadlessMemberExpr(_, name)    ⇒ Abstract.HeadlessMemberExpr(name.text, memberExpr.isNullSafe, sourceInfo(expr))
    case Concrete.LookupExpr(e, _, index, _)                ⇒ Abstract.LookupExpr(abstractify(e), abstractify(index), sourceInfo(expr))
    case Concrete.ParenExpr(_, e, _)                        ⇒ Abstract.ParenExpr(abstractify(e), sourceInfo(expr))
    case Concrete.BlockExpr(_, statements, _)               ⇒ Abstract.BlockExpr(abstractify(statements), sourceInfo(expr))
    case Concrete.StatementSeq(statements)                  ⇒ Abstract.StatementSeq(statements.flatMap(_.statementOpt).map(abstractify), sourceInfo(expr))
    case Concrete.LambdaExpr(params, _, body)               ⇒ Abstract.LambdaExpr(abstractifyParamList(params), abstractify(body), sourceInfo(expr))
    case Concrete.BinOpExpr(left, opToken, right)           ⇒ Abstract.BinOpExpr(abstractify(left), getBinaryOperator(opToken), abstractify(right), sourceInfo(expr))
    case assignmentExpr: Concrete.AssignmentExpr            ⇒ abstractifyAssignmentExpr(assignmentExpr)
    case assignmentExpr: Concrete.PatternAssignmentExpr     ⇒ abstractifyPatternAssignmentExpr(assignmentExpr)
    case chainedExpr@Concrete.ChainedOpExpr(_, _)           ⇒ abstractifyChainedComparison(chainedExpr)
    case iExpr@Concrete.InvocationExpr(_, _)                ⇒ abstractifyInvocation(iExpr)
    case iExpr@Concrete.ParenInvocationExpr(_, _, _, _)     ⇒ abstractifyParenInvocation(iExpr)
    case listExpr@Concrete.ListExpr(_, _, _)                ⇒ abstractifyList(listExpr)
    case objectExpr@Concrete.ObjectExpr(_, _, _)            ⇒ abstractifyObject(objectExpr)
    case Concrete.IfExpr(_, cond, _, body, elseOpt)         ⇒ Abstract.IfExpr(abstractify(cond), abstractify(body), elseOpt.map { case (_, elseBody) ⇒ abstractify(elseBody) }, sourceInfo(expr))
    case Concrete.MinusExpr(_, subExpr)                     ⇒ Abstract.MinusExpr(abstractify(subExpr), sourceInfo(expr))
    case mishExpr: Concrete.MishExpr                        ⇒ abstractifyMish(mishExpr, captureProcessOutput = false)
    case str: Concrete.InterpolatedString                   ⇒ abstractifyInterpolatedString(str)
    case decl: Concrete.FunctionDeclaration                 ⇒ abstractifyFunctionDeclaration(decl)
    case decl: Concrete.ClassDeclaration                    ⇒ abstractifyClassDeclaration(decl)
    case Concrete.MishFunction(word)                        ⇒ Abstract.MishFunction(word.text.tail, sourceInfo(expr))
    case Concrete.HelpExpr(subExpr, _)                      ⇒ Abstract.HelpExpr(abstractify(subExpr), sourceInfo(expr))
    case Concrete.MishInterpolationExpr(start, mishExpr, _) ⇒ abstractifyMish(mishExpr, captureProcessOutput = start.tokenType == MISH_INTERPOLATION_START)
    case Concrete.ThisExpr(thisToken)                       ⇒ Abstract.ThisExpr(sourceInfo(expr))
  }

  private def abstractifyPatternAssignmentExpr(assignmentExpr: Concrete.PatternAssignmentExpr): Abstract.PatternAssignmentExpr = {
    val Concrete.PatternAssignmentExpr(pattern, _, right) = assignmentExpr
    Abstract.PatternAssignmentExpr(abstractifyPattern(pattern), abstractify(right), sourceInfo(assignmentExpr))
  }

  private def abstractifyAssignmentExpr(assignmentExpr: Concrete.AssignmentExpr): Abstract.AssignmentExpr = {
    val Concrete.AssignmentExpr(left, equalsToken, right) = assignmentExpr
    val op = getAssignmentBinaryOperator(equalsToken)
    Abstract.AssignmentExpr(abstractify(left), op, abstractify(right), sourceInfo(assignmentExpr))
  }

  private def getStringText(s: String, maybeTilde: Boolean, pruneInitial: Boolean, pruneFinal: Boolean): (String, Boolean) = {
    def dropInitialQuote(s: String) = if (pruneInitial && (s.startsWith("\"") || s.startsWith("'"))) s.tail else s
    def dropFinalQuote(s: String) = if (pruneFinal && (s.endsWith("\"") || s.endsWith("'"))) s.init else s
    val withoutQuotes = dropInitialQuote(dropFinalQuote(s))
    val hasTildePrefix = maybeTilde && withoutQuotes.startsWith("~")
    val withoutTilde = if (hasTildePrefix) withoutQuotes.tail else withoutQuotes
    (StringEscapes.unescape(withoutTilde), hasTildePrefix)
  }

  private def abstractifyLiteral(token: Token, sourceInfoOpt: Option[SourceInfo]): Abstract.Expr =
    token.tokenType match {
      case TokenType.NUMBER_LITERAL ⇒ Abstract.Literal(MashNumber(token.text.toDouble), sourceInfoOpt)
      case TokenType.TRUE           ⇒ Abstract.Literal(MashBoolean.True, sourceInfoOpt)
      case TokenType.FALSE          ⇒ Abstract.Literal(MashBoolean.False, sourceInfoOpt)
      case TokenType.NULL           ⇒ Abstract.Literal(MashNull, sourceInfoOpt)
      case TokenType.STRING_LITERAL ⇒
        val s = token.text
        val quotationType = if (s.startsWith("\"")) QuotationType.Double else QuotationType.Single
        val (literalText, hasTildePrefix) = getStringText(s, maybeTilde = quotationType == QuotationType.Double,
          pruneInitial = true, pruneFinal = true)
        Abstract.StringLiteral(literalText, quotationType, hasTildePrefix, sourceInfoOpt)
      case _                        ⇒
        throw new RuntimeException("Unexpected token type: " + token.tokenType)
    }

  private def abstractifyFunctionDeclaration(decl: Concrete.FunctionDeclaration): Abstract.FunctionDeclaration = {
    val Concrete.FunctionDeclaration(attributesOpt ,_, _, name, params, _, body) = decl
    val abstractParams = abstractifyParamList(params)
    val docCommentOpt = decl.docCommentOpt.flatMap(dc ⇒ DocCommentParser.parse(dc.text))
    val attributes = attributesOpt.map(abstractifyAttributes).getOrElse(Seq())
    Abstract.FunctionDeclaration(attributes, docCommentOpt, name.text, abstractParams, abstractify(body), sourceInfo(decl))
  }

  private def abstractifyAttributes(attributes: Concrete.Attributes): Seq[Abstract.Attribute] =
    attributes.attributes.map(attr ⇒ Abstract.Attribute(attr.name.text, sourceInfo(attr)))

  private def abstractifyClassDeclaration(decl: Concrete.ClassDeclaration): Abstract.ClassDeclaration = {
    val Concrete.ClassDeclaration(_, _, name, params, _) = decl
    val abstractParams = abstractifyParamList(params)
    val abstractBodyOpt = decl.bodyOpt.map(abstractifyClassBody)
    val docCommentOpt = decl.docCommentOpt.flatMap(dc ⇒ DocCommentParser.parse(dc.text))
    Abstract.ClassDeclaration(docCommentOpt, name.text, abstractParams, abstractBodyOpt, sourceInfo(decl))
  }

  private def abstractifyClassBody(body: Concrete.ClassBody): Abstract.ClassBody =
    Abstract.ClassBody(body.methods.map(method ⇒ abstractifyFunctionDeclaration(method.methodDeclaration)))

  private def abstractifyParam(param: Concrete.Param): Abstract.FunctionParam = param match {
    case Concrete.ParenParam(_, attributesOpt, childParam, _) ⇒
      val attributes = attributesOpt.map(abstractifyAttributes(_)).getOrElse(Seq())
      abstractifyParam(childParam).copy(attributes = attributes)
    case Concrete.PatternParam(pattern, ellipsisOpt, equalsDefaultOpt)                 ⇒
      val defaultExprOpt = equalsDefaultOpt.map { case (_, defaultExpr) ⇒ abstractify(defaultExpr) }
      Abstract.FunctionParam(Seq(), pattern.nameOpt, sourceInfoOpt = sourceInfo(pattern),
        patternOpt = Some(abstractifyPattern(pattern)), isVariadic = ellipsisOpt.isDefined,
        defaultExprOpt = defaultExprOpt)
  }

  private def abstractifyObjectPatternEntry(entry: Concrete.ObjectPatternEntry): Abstract.ObjectPatternEntry = entry match {
    case Concrete.ShorthandObjectPatternEntry(identifier)        ⇒
      Abstract.ObjectPatternEntry(identifier.text, None, sourceInfoOpt = sourceInfo(entry))
    case Concrete.FullObjectPatternEntry(identifier, _, pattern) ⇒
      Abstract.ObjectPatternEntry(identifier.text, Some(abstractifyPattern(pattern)), sourceInfoOpt = sourceInfo(entry))
  }

  private def abstractifyObjectPattern(pattern: Concrete.ObjectPattern): Abstract.ObjectPattern = {
    val Concrete.ObjectPattern(_, contentsOpt, _) = pattern
    val entries = contentsOpt.map { contents ⇒
      val firstEntry = abstractifyObjectPatternEntry(contents.firstEntry)
      val otherEntries = contents.otherEntries.map { case (_, item) ⇒ abstractifyObjectPatternEntry(item) }
      firstEntry +: otherEntries
    }.getOrElse(Seq())
    Abstract.ObjectPattern(entries, sourceInfoOpt = sourceInfo(pattern))
  }

  private def abstractifyListPattern(pattern: Concrete.ListPattern): Abstract.ListPattern = {
    val elements = pattern.contentsOpt.map { contents ⇒
      val firstEntry = abstractifyPattern(contents.firstElement)
      val otherEntries = contents.otherElements.map { case (_, item) ⇒ abstractifyPattern(item) }
      firstEntry +: otherEntries
    }.getOrElse(Seq())
    Abstract.ListPattern(elements)
  }

  private def abstractifyPattern(pattern: Concrete.Pattern): Abstract.Pattern = pattern match {
    case objectPattern: Concrete.ObjectPattern ⇒ abstractifyObjectPattern(objectPattern)
    case listPattern: Concrete.ListPattern     ⇒ abstractifyListPattern(listPattern)
    case Concrete.HolePattern(_)               ⇒ Abstract.HolePattern(sourceInfoOpt = sourceInfo(pattern))
    case Concrete.IdentPattern(identifier)     ⇒ Abstract.IdentPattern(identifier.text, sourceInfoOpt = sourceInfo(pattern))
  }

  private def abstractifyParamList(params: Concrete.ParamList): Abstract.ParamList = {
    val abstractParams = params.params.map(abstractifyParam)
    Abstract.ParamList(abstractParams)
  }

  private def abstractifyInterpolatedString(interpolatedString: Concrete.InterpolatedString) = {
    val Concrete.InterpolatedString(start, parts, end) = interpolatedString
    val newParts = parts.map(abstractifyInterpolationPart)
    val (startText, _) = getStringText(start.text, maybeTilde = true, pruneInitial = true, pruneFinal = false)
    val (endText, _) = getStringText(end.text, maybeTilde = false, pruneInitial = false, pruneFinal = true)
    Abstract.InterpolatedString(startText, newParts, endText, sourceInfo(interpolatedString))
  }

  private def abstractifyInterpolationPart(part: Concrete.InterpolationPart): Abstract.InterpolationPart = part match {
    case Concrete.SimpleInterpolation(_, subExpr)     ⇒
      Abstract.ExprPart(abstractify(subExpr))
    case Concrete.ComplexInterpolation(_, subExpr, _) ⇒
      Abstract.ExprPart(abstractify(subExpr))
    case Concrete.StringPart(stringMiddle)            ⇒
      val (literalText, _) = getStringText(stringMiddle.text, maybeTilde = false, pruneInitial = false, pruneFinal = false)
      Abstract.StringPart(literalText)
  }

  private def abstractifyMish(expr: Concrete.MishExpr, captureProcessOutput: Boolean): Abstract.MishExpr = {
    val Concrete.MishExpr(command, args) = expr
    val redirects = args.collect {
      case Concrete.MishRedirect(token, arg) ⇒
        val operator =
          if (token.tokenType == GREATER_THAN)
            RedirectOperator.StandardOutput
          else
            RedirectOperator.StandardInput
        Abstract.MishRedirect(operator, abstractifyMishItem(arg).get)
    }
    Abstract.MishExpr(abstractifyMishItem(command).get, args.flatMap(abstractifyMishItem), redirects, captureProcessOutput, sourceInfo(expr))
  }

  private def abstractifyMishItem(item: Concrete.MishItem): Option[Abstract.Expr] = condOpt(item) {
    case Concrete.MishInterpolation(part) ⇒
      Abstract.MishInterpolation(abstractifyInterpolationPart(part))
    case Concrete.MishString(expr)        ⇒
      abstractify(expr)
    case Concrete.MishWord(token)         ⇒
      Abstract.StringLiteral(token.text, QuotationType.Double)
  }

  private def abstractifyEntry(entry: Concrete.ObjectEntry): Abstract.ObjectEntry = entry match {
    case fullEntry: Concrete.FullObjectEntry           ⇒ abstractifyEntry(fullEntry)
    case shorthandEntry: Concrete.ShorthandObjectEntry ⇒ abstractifyEntry(shorthandEntry)
  }

  private def abstractifyEntry(entry: Concrete.ShorthandObjectEntry): Abstract.ShorthandObjectEntry =
    Abstract.ShorthandObjectEntry(entry.identifier.text, sourceInfo(entry))

  private def abstractifyEntry(entry: Concrete.FullObjectEntry): Abstract.FullObjectEntry =
    Abstract.FullObjectEntry(abstractify(entry.field), abstractify(entry.value), sourceInfo(entry))

  private def abstractifyObject(objectExpr: Concrete.ObjectExpr): Abstract.ObjectExpr = {
    val fields =
      objectExpr.contentsOpt match {
        case Some(Concrete.ObjectExprContents(firstEntry, otherEntries)) ⇒
          abstractifyEntry(firstEntry) +: otherEntries.map(_._2).map(abstractifyEntry)
        case None                                                        ⇒ Seq()
      }

    Abstract.ObjectExpr(fields, sourceInfo(objectExpr))
  }

  private def abstractifyList(listExpr: Concrete.ListExpr): Abstract.ListExpr = {
    val elements =
      listExpr.contentsOpt match {
        case Some(Concrete.ListExprContents(firstElement, otherElements)) ⇒
          abstractify(firstElement) +: otherElements.map(_._2).map(abstractify)
        case None                                                         ⇒
          Seq()
      }
    Abstract.ListExpr(elements, sourceInfo(listExpr))
  }

  private def abstractifyParenInvocation(invocationExpr: Concrete.ParenInvocationExpr): Abstract.Expr = {
    val Concrete.ParenInvocationExpr(function, _, argsOpt, _) = invocationExpr
    def abstractifyArg(expr: Concrete.Expr) = abstractify(expr)
    val args: Seq[Argument] =
      (argsOpt match {
        case Some(Concrete.ParenInvocationArgs(firstArg, otherArgs)) ⇒
          abstractifyArg(firstArg) +: otherArgs.map(_._2).map(abstractifyArg)
        case None                                                    ⇒
          Seq()
      }).map(arg ⇒ Argument.PositionArg(arg, arg.sourceInfoOpt))
    Abstract.InvocationExpr(abstractify(function), args, isParenInvocation = true, sourceInfo(invocationExpr))
  }

  private def abstractifyInvocation(invocationExpr: Concrete.InvocationExpr): Abstract.InvocationExpr = {
    val Concrete.InvocationExpr(function, args) = invocationExpr
    val abstractArgs: Seq[Argument] = args map {
      case arg@Concrete.ShortArg(flag)                  ⇒
        Argument.ShortFlag(flag.text.drop(1).map(_.toString), sourceInfo(arg))
      case arg@Concrete.LongArg(flag, None)             ⇒
        Argument.LongFlag(flag.text.drop(2), None, sourceInfo(arg))
      case arg@Concrete.LongArg(flag, Some((_, value))) ⇒
        Argument.LongFlag(flag.text.drop(2), Some(abstractify(value)), sourceInfo(arg))
      case e: Concrete.Expr                             ⇒
        Argument.PositionArg(abstractify(e), sourceInfo(e))
      case x                                            ⇒
        throw new RuntimeException("Unexpected argument: " + x)
    }
    Abstract.InvocationExpr(abstractify(function), abstractArgs, isParenInvocation = false, sourceInfo(invocationExpr))
  }

  private def abstractifyChainedComparison(chainedExpr: Concrete.ChainedOpExpr): Abstract.Expr = {
    val Concrete.ChainedOpExpr(left, opRights) = chainedExpr
    Abstract.ChainedOpExpr(abstractify(left), opRights.map {
      case (opToken, right) ⇒ (getBinaryOperator(opToken), abstractify(right))
    }, sourceInfo(chainedExpr))
  }

  private def getAssignmentBinaryOperator(token: Token): Option[BinaryOperator] = condOpt(token.tokenType) {
    case TokenType.PLUS_EQUALS   ⇒ BinaryOperator.Plus
    case TokenType.MINUS_EQUALS  ⇒ BinaryOperator.Minus
    case TokenType.TIMES_EQUALS  ⇒ BinaryOperator.Multiply
    case TokenType.DIVIDE_EQUALS ⇒ BinaryOperator.Divide
  }

  private def getBinaryOperator(token: Token): BinaryOperator =
    token.tokenType match {
      case TokenType.AND                 ⇒ BinaryOperator.And
      case TokenType.OR                  ⇒ BinaryOperator.Or
      case TokenType.PLUS                ⇒ BinaryOperator.Plus
      case TokenType.MINUS               ⇒ BinaryOperator.Minus
      case TokenType.TIMES               ⇒ BinaryOperator.Multiply
      case TokenType.DIVIDE              ⇒ BinaryOperator.Divide
      case TokenType.LONG_EQUALS         ⇒ BinaryOperator.Equals
      case TokenType.NOT_EQUALS          ⇒ BinaryOperator.NotEquals
      case TokenType.GREATER_THAN_EQUALS ⇒ BinaryOperator.GreaterThanEquals
      case TokenType.GREATER_THAN        ⇒ BinaryOperator.GreaterThan
      case TokenType.LESS_THAN           ⇒ BinaryOperator.LessThan
      case TokenType.LESS_THAN_EQUALS    ⇒ BinaryOperator.LessThanEquals
      case TokenType.SEMI                ⇒ BinaryOperator.Sequence
      case _                             ⇒ throw new RuntimeException("Unexpected token type: " + token.tokenType)
    }

  private def sourceInfo(node: Concrete.AstNode) = Some(SourceInfo(provenance, node))

}