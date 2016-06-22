package com.github.mdr.mash.parser

import scala.collection.immutable.ListMap

import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.AbstractSyntax.Argument
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashNumber

/**
 * Convert from concrete to abstract syntax trees.
 */
class Abstractifier(provenance: Provenance) {
  import com.github.mdr.mash.parser.{ ConcreteSyntax ⇒ Concrete, AbstractSyntax ⇒ Abstract }

  def abstractify(expr: Concrete.Expr): Abstract.Expr = expr match {
    case Concrete.Literal(token)                            ⇒ abstractifyLiteral(token, sourceInfo(expr))
    case Concrete.Identifier(token)                         ⇒ Abstract.Identifier(token.text, sourceInfo(expr))
    case Concrete.Hole(_)                                   ⇒ Abstract.Hole(sourceInfo(expr))
    case Concrete.PipeExpr(left, _, right)                  ⇒ Abstract.PipeExpr(abstractify(left), abstractify(right), sourceInfo(expr))
    case mexpr @ Concrete.MemberExpr(e, _, name)            ⇒ Abstract.MemberExpr(abstractify(e), name.text, mexpr.isNullSafe, sourceInfo(expr))
    case mexpr @ Concrete.HeadlessMemberExpr(_, name)       ⇒ Abstract.HeadlessMemberExpr(name.text, mexpr.isNullSafe, sourceInfo(expr))
    case Concrete.LookupExpr(e, _, index, _)                ⇒ Abstract.LookupExpr(abstractify(e), abstractify(index), sourceInfo(expr))
    case Concrete.ParenExpr(_, e, _)                        ⇒ Abstract.ParenExpr(abstractify(e), sourceInfo(expr))
    case Concrete.BlockExpr(_, statements, _)               ⇒ abstractify(statements)
    case Concrete.StatementSeq(statements)                  ⇒ Abstract.StatementSeq(statements.flatMap(_.statementOpt).map(abstractify), sourceInfo(expr))
    case Concrete.LambdaExpr(param, _, body)                ⇒ Abstract.LambdaExpr(param.text, abstractify(body), sourceInfo(expr))
    case Concrete.BinOpExpr(left, opToken, right)           ⇒ Abstract.BinOpExpr(abstractify(left), getBinaryOperator(opToken), abstractify(right), sourceInfo(expr))
    case Concrete.AssignmentExpr(left, _, aliasOpt, right)  ⇒ Abstract.AssignmentExpr(abstractify(left), abstractify(right), aliasOpt.isDefined, sourceInfo(expr))
    case chainedExpr @ Concrete.ChainedOpExpr(_, _)         ⇒ abstractifyChainedComparision(chainedExpr)
    case iExpr @ Concrete.InvocationExpr(_, _)              ⇒ abstractifyInvocation(iExpr)
    case iExpr @ Concrete.ParenInvocationExpr(_, _, _, _)   ⇒ abstractifyParenInvocation(iExpr)
    case listExpr @ Concrete.ListExpr(_, _, _)              ⇒ abstractifyList(listExpr)
    case objectExpr @ Concrete.ObjectExpr(_, _, _)          ⇒ abstractifyObject(objectExpr)
    case Concrete.IfExpr(_, cond, _, body, elseOpt)         ⇒ Abstract.IfExpr(abstractify(cond), abstractify(body), elseOpt.map { case (_, elseBody) ⇒ abstractify(elseBody) }, sourceInfo(expr))
    case Concrete.MinusExpr(_, subExpr)                     ⇒ Abstract.MinusExpr(abstractify(subExpr), sourceInfo(expr))
    case mishExpr: Concrete.MishExpr                        ⇒ abstractifyMish(mishExpr, captureProcessOutput = false)
    case str: Concrete.InterpolatedString                   ⇒ abstractifyInterpolatedString(str)
    case decl: Concrete.FunctionDeclaration                 ⇒ abstractifyFunctionDeclaration(decl)
    case Concrete.MishFunction(word)                        ⇒ Abstract.MishFunction(word.text.tail, sourceInfo(expr))
    case Concrete.HelpExpr(expr, _)                         ⇒ Abstract.HelpExpr(abstractify(expr), sourceInfo(expr))
    case Concrete.MishInterpolationExpr(start, mishExpr, _) ⇒ abstractifyMish(mishExpr, captureProcessOutput = start.tokenType == MISH_INTERPOLATION_START)
  }

  private def getStringText(s: String, maybeTilde: Boolean): (String, Boolean) = {
    def dropInitialQuote(s: String) = if (s.startsWith("\"") || s.startsWith("'")) s.tail else s
    def dropFinalQuote(s: String) = if (s.endsWith("\"") || s.endsWith("'")) s.init else s
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
        val (literalText, hasTildePrefix) = getStringText(s, maybeTilde = quotationType == QuotationType.Double)
        Abstract.StringLiteral(literalText, quotationType, hasTildePrefix, sourceInfoOpt)
      case _ ⇒
        throw new RuntimeException("Unexpected token type: " + token.tokenType)
    }

  private def abstractifyFunctionDeclaration(decl: Concrete.FunctionDeclaration): Abstract.FunctionDeclaration = {
    val Concrete.FunctionDeclaration(_, name, params, _, body) = decl
    val abstractParams = params.map {
      case param @ Concrete.SimpleParam(name)      ⇒ Abstract.SimpleParam(name.text, sourceInfo(param))
      case param @ Concrete.VariadicParam(name, _) ⇒ Abstract.VariadicParam(name.text, sourceInfo(param))
    }
    Abstract.FunctionDeclaration(name.text, abstractParams, abstractify(body), sourceInfo(decl))
  }

  private def abstractifyInterpolatedString(interpolatedString: Concrete.InterpolatedString) = {
    val Concrete.InterpolatedString(start, parts, end) = interpolatedString
    val newParts = parts.map(abstractifyInterpolationPart)
    val (startText, _) = getStringText(start.text, maybeTilde = true)
    val (endText, _) = getStringText(end.text, maybeTilde = false)
    Abstract.InterpolatedString(startText, newParts, endText, sourceInfo(interpolatedString))
  }

  private def abstractifyInterpolationPart(part: Concrete.InterpolationPart): Abstract.InterpolationPart = part match {
    case Concrete.SimpleInterpolation(_, subExpr) ⇒
      Abstract.ExprPart(abstractify(subExpr))
    case Concrete.ComplexInterpolation(_, subExpr, _) ⇒
      Abstract.ExprPart(abstractify(subExpr))
    case Concrete.StringPart(stringMiddle) ⇒
      val (literalText, _) = getStringText(stringMiddle.text, maybeTilde = false)
      Abstract.StringPart(literalText)
  }

  private def abstractifyMish(expr: Concrete.MishExpr, captureProcessOutput: Boolean): Abstract.MishExpr = {
    val Concrete.MishExpr(command, args) = expr
    Abstract.MishExpr(abstractifyMishItem(command), args.map(abstractifyMishItem), captureProcessOutput, sourceInfo(expr))
  }

  private def abstractifyMishItem(item: Concrete.MishItem): Abstract.Expr = item match {
    case Concrete.MishInterpolation(part) ⇒
      Abstract.MishInterpolation(abstractifyInterpolationPart(part))
    case Concrete.MishString(expr) ⇒
      abstractify(expr)
    case Concrete.MishWord(token) ⇒
      Abstract.StringLiteral(token.text, QuotationType.Double)
  }

  private def abstractifyEntry(entry: Concrete.ObjectEntry): (String, Abstract.Expr) =
    entry.fieldLabel.text -> abstractify(entry.value)

  private def abstractifyObject(objectExpr: Concrete.ObjectExpr): Abstract.ObjectExpr = {
    val Concrete.ObjectExpr(lbrace, contentsOpt, rbrace) = objectExpr
    val fieldToExprs =
      contentsOpt match {
        case Some(Concrete.ObjectExprContents(firstEntry, otherEntries)) ⇒ abstractifyEntry(firstEntry) +: otherEntries.map(_._2).map(abstractifyEntry)
        case None ⇒ Seq()
      }
    Abstract.ObjectExpr(ListMap(fieldToExprs: _*), sourceInfo(objectExpr))
  }

  private def abstractifyList(listExpr: Concrete.ListExpr): Abstract.ListExpr = {
    val Concrete.ListExpr(lsquare, contentsOpt, rsquare) = listExpr
    val items =
      contentsOpt match {
        case Some(Concrete.ListExprContents(firstItem, otherItems)) ⇒ abstractify(firstItem) +: otherItems.map(_._2).map(abstractify)
        case None ⇒ Seq()
      }
    Abstract.ListExpr(items, sourceInfo(listExpr))
  }

  private def abstractifyParenInvocation(invocationExpr: Concrete.ParenInvocationExpr): Abstract.Expr = {
    val Concrete.ParenInvocationExpr(function, _, argsOpt, _) = invocationExpr
    def abstractifyArg(expr: Concrete.Expr) = abstractify(expr)
    val args: Seq[Argument] =
      (argsOpt match {
        case Some(Concrete.ParenInvocationArgs(firstArg, otherArgs)) ⇒
          abstractifyArg(firstArg) +: otherArgs.map(_._2).map(abstractifyArg)
        case None ⇒
          Seq()
      }).map(arg ⇒ Argument.PositionArg(arg, arg.sourceInfoOpt))
    Abstract.InvocationExpr(abstractify(function), args, isParenInvocation = true, sourceInfo(invocationExpr))
  }

  private def abstractifyInvocation(invocationExpr: Concrete.InvocationExpr): Abstract.InvocationExpr = {
    val Concrete.InvocationExpr(function, args) = invocationExpr
    val abstractArgs: Seq[Argument] = args map {
      case arg @ Concrete.ShortArg(flag) ⇒
        Argument.ShortFlag(flag.text.drop(1).map(_.toString), sourceInfo(arg))
      case arg @ Concrete.LongArg(flag, None) ⇒
        Argument.LongFlag(flag.text.drop(2), None, sourceInfo(arg))
      case arg @ Concrete.LongArg(flag, Some((_, value))) ⇒
        Argument.LongFlag(flag.text.drop(2), Some(abstractify(value)), sourceInfo(arg))
      case e: Concrete.Expr ⇒
        Argument.PositionArg(abstractify(e), sourceInfo(e))
      case x ⇒
        throw new RuntimeException("Unexpected argument: " + x)
    }
    Abstract.InvocationExpr(abstractify(function), abstractArgs, isParenInvocation = false, sourceInfo(invocationExpr))
  }

  private def abstractifyChainedComparision(chainedExpr: Concrete.ChainedOpExpr): Abstract.Expr = {
    val Concrete.ChainedOpExpr(left, opRights) = chainedExpr
    Abstract.ChainedOpExpr(abstractify(left), opRights.map {
      case (opToken, right) ⇒ (getBinaryOperator(opToken), abstractify(right))
    }, sourceInfo(chainedExpr))
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