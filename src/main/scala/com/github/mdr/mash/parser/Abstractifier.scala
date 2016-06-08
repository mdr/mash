package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashString
import scala.collection.immutable.ListMap
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.parser.AbstractSyntax.Argument
import com.github.mdr.mash.lexer.TokenType.MISH_INTERPOLATION_START
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashBoolean

/**
 * Convert from concrete to abstract syntax trees.
 */
object Abstractifier {
  import com.github.mdr.mash.parser.{ ConcreteSyntax ⇒ Concrete, AbstractSyntax ⇒ Abstract }

  private def getStringText(s: String, maybeTilde: Boolean): (String, Boolean) = {
    def dropInitialQuote(s: String) = if (s.startsWith("\"") || s.startsWith("'")) s.tail else s
    def dropFinalQuote(s: String) = if (s.endsWith("\"") || s.endsWith("'")) s.init else s
    val withoutQuotes = dropInitialQuote(dropFinalQuote(s))
    val tildePrefix = maybeTilde && withoutQuotes.startsWith("~")
    val withoutTilde = if (tildePrefix) withoutQuotes.tail else withoutQuotes
    (StringEscapes.unescape(withoutTilde), tildePrefix)
  }

  private def abstractifyLiteral(token: Token, sourceInfo: SourceInfo): Abstract.Expr =
    token.tokenType match {
      case TokenType.NUMBER_LITERAL ⇒ Abstract.Literal(MashNumber(token.text.toDouble), Some(sourceInfo))
      case TokenType.TRUE           ⇒ Abstract.Literal(MashBoolean.True, Some(sourceInfo))
      case TokenType.FALSE          ⇒ Abstract.Literal(MashBoolean.False, Some(sourceInfo))
      case TokenType.NULL           ⇒ Abstract.Literal(MashNull, Some(sourceInfo))
      case TokenType.STRING_LITERAL ⇒
        val s = token.text
        val quotationType = if (s.startsWith("\"")) QuotationType.Double else QuotationType.Single
        val (literalText, tildePrefix) = getStringText(s, maybeTilde = quotationType == QuotationType.Double)
        Abstract.StringLiteral(literalText, quotationType, tildePrefix, Some(sourceInfo))
      case _ ⇒
        throw new RuntimeException("Unexpected token type: " + token.tokenType)
    }

  def abstractify(expr: Concrete.Expr): Abstract.Expr = expr match {
    case Concrete.Literal(token)                            ⇒ abstractifyLiteral(token, SourceInfo(expr))
    case Concrete.Identifier(token)                         ⇒ Abstract.Identifier(token.text, Some(SourceInfo(expr)))
    case Concrete.Hole(_)                                   ⇒ Abstract.Hole(Some(SourceInfo(expr)))
    case Concrete.PipeExpr(left, _, right)                  ⇒ Abstract.PipeExpr(abstractify(left), abstractify(right), Some(SourceInfo(expr)))
    case mexpr @ Concrete.MemberExpr(e, _, name)            ⇒ Abstract.MemberExpr(abstractify(e), name.text, mexpr.isNullSafe, Some(SourceInfo(expr)))
    case mexpr @ Concrete.HeadlessMemberExpr(_, name)       ⇒ Abstract.HeadlessMemberExpr(name.text, mexpr.isNullSafe, Some(SourceInfo(expr)))
    case Concrete.LookupExpr(e, _, index, _)                ⇒ Abstract.LookupExpr(abstractify(e), abstractify(index), Some(SourceInfo(expr)))
    case Concrete.ParenExpr(_, e, _)                        ⇒ Abstract.ParenExpr(abstractify(e), Some(SourceInfo(expr)))
    case Concrete.BlockExpr(_, statements, _)               ⇒ abstractify(statements)
    case Concrete.StatementSeq(statements)                  ⇒ Abstract.StatementSeq(statements.flatMap(_.statementOpt).map(abstractify), Some(SourceInfo(expr)))
    case Concrete.LambdaExpr(param, _, body)                ⇒ Abstract.LambdaExpr(param.text, abstractify(body), Some(SourceInfo(expr)))
    case Concrete.BinOpExpr(left, opToken, right)           ⇒ Abstract.BinOpExpr(abstractify(left), getBinaryOperator(opToken), abstractify(right), Some(SourceInfo(expr)))
    case Concrete.AssignmentExpr(left, _, aliasOpt, right)  ⇒ Abstract.AssignmentExpr(abstractify(left), abstractify(right), aliasOpt.isDefined, Some(SourceInfo(expr)))
    case chainedExpr @ Concrete.ChainedOpExpr(_, _)         ⇒ abstractifyChainedComparision(chainedExpr)
    case iExpr @ Concrete.InvocationExpr(_, _)              ⇒ abstractifyInvocation(iExpr)
    case iExpr @ Concrete.ParenInvocationExpr(_, _, _, _)   ⇒ abstractifyParenInvocation(iExpr)
    case listExpr @ Concrete.ListExpr(_, _, _)              ⇒ abstractifyList(listExpr)
    case objectExpr @ Concrete.ObjectExpr(_, _, _)          ⇒ abstractifyObject(objectExpr)
    case Concrete.IfExpr(_, cond, _, body, elseOpt)         ⇒ Abstract.IfExpr(abstractify(cond), abstractify(body), elseOpt.map { case (_, elseBody) ⇒ abstractify(elseBody) }, Some(SourceInfo(expr)))
    case Concrete.MinusExpr(_, subExpr)                     ⇒ Abstract.MinusExpr(abstractify(subExpr), Some(SourceInfo(expr)))
    case mishExpr: Concrete.MishExpr                        ⇒ abstractifyMish(mishExpr, captureProcessOutput = false)
    case str: Concrete.InterpolatedString                   ⇒ abstractifyInterpolatedString(str)
    case decl: Concrete.FunctionDeclaration                 ⇒ abstractifyFunctionDeclaration(decl)
    case Concrete.MishFunction(word)                        ⇒ Abstract.MishFunction(word.text.tail, Some(SourceInfo(expr)))
    case Concrete.HelpExpr(expr, _)                         ⇒ Abstract.HelpExpr(abstractify(expr), Some(SourceInfo(expr)))
    case Concrete.MishInterpolationExpr(start, mishExpr, _) ⇒ abstractifyMish(mishExpr, captureProcessOutput = start.tokenType == MISH_INTERPOLATION_START)
  }

  private def abstractifyFunctionDeclaration(decl: Concrete.FunctionDeclaration): Abstract.FunctionDeclaration = {
    val Concrete.FunctionDeclaration(_, name, params, _, body) = decl
    val abstractParams = params.map {
      case param @ Concrete.SimpleParam(name)      ⇒ Abstract.SimpleParam(name.text, Some(SourceInfo(param)))
      case param @ Concrete.VariadicParam(name, _) ⇒ Abstract.VariadicParam(name.text, Some(SourceInfo(param)))
    }
    Abstract.FunctionDeclaration(name.text, abstractParams, abstractify(body), Some(SourceInfo(decl)))
  }

  private def abstractifyInterpolatedString(interpolatedString: Concrete.InterpolatedString) = {
    val Concrete.InterpolatedString(start, parts, end) = interpolatedString
    val newParts = parts.map(abstractifyInterpolationPart)
    val (startText, _) = getStringText(start.text, maybeTilde = true)
    val (endText, _) = getStringText(end.text, maybeTilde = false)
    Abstract.InterpolatedString(startText, newParts, endText, Some(SourceInfo(interpolatedString)))
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
    Abstract.MishExpr(abstractifyMishItem(command), args.map(abstractifyMishItem), captureProcessOutput, Some(SourceInfo(expr)))
  }

  private def abstractifyMishItem(item: Concrete.MishItem): Abstract.Expr = item match {
    case Concrete.MishInterpolation(part) ⇒
      Abstract.MishInterpolation(abstractifyInterpolationPart(part))
    case Concrete.MishString(expr) ⇒
      abstractify(expr)
    case Concrete.MishWord(token) ⇒
      Abstract.StringLiteral(token.text, QuotationType.Double, tildePrefix = false, sourceInfoOpt = None)
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
    Abstract.ObjectExpr(ListMap(fieldToExprs: _*), Some(SourceInfo(objectExpr)))
  }

  private def abstractifyList(listExpr: Concrete.ListExpr): Abstract.ListExpr = {
    val Concrete.ListExpr(lsquare, contentsOpt, rsquare) = listExpr
    val items =
      contentsOpt match {
        case Some(Concrete.ListExprContents(firstItem, otherItems)) ⇒ abstractify(firstItem) +: otherItems.map(_._2).map(abstractify)
        case None ⇒ Seq()
      }
    Abstract.ListExpr(items, Some(SourceInfo(listExpr)))
  }

  private def abstractifyParenInvocation(invocationExpr: Concrete.ParenInvocationExpr): Abstract.Expr = {
    val Concrete.ParenInvocationExpr(function, _, argsOpt, _) = invocationExpr
    def abstractifyArg(expr: Concrete.Expr) = abstractify(expr) //Abstract.ParenExpr(abstractify(expr), Some(SourceInfo(expr)))
    val args: Seq[Argument] =
      (argsOpt match {
        case Some(Concrete.ParenInvocationArgs(firstArg, otherArgs)) ⇒
          abstractifyArg(firstArg) +: otherArgs.map(_._2).map(abstractifyArg)
        case None ⇒
          Seq()
      }).map(arg ⇒ Argument.PositionArg(arg, arg.sourceInfoOpt))
    Abstract.InvocationExpr(abstractify(function), args, Some(SourceInfo(invocationExpr)))
  }

  private def abstractifyInvocation(invocationExpr: Concrete.InvocationExpr): Abstract.InvocationExpr = {
    val Concrete.InvocationExpr(function, args) = invocationExpr
    val abstractArgs: Seq[Argument] = args map {
      case arg @ Concrete.ShortArg(flag) ⇒
        Argument.ShortFlag(flag.text.drop(1).map(_.toString), Some(SourceInfo(arg)))
      case arg @ Concrete.LongArg(flag, None) ⇒
        Argument.LongFlag(flag.text.drop(2), None, Some(SourceInfo(arg)))
      case arg @ Concrete.LongArg(flag, Some((_, value))) ⇒
        Argument.LongFlag(flag.text.drop(2), Some(abstractify(value)), Some(SourceInfo(arg)))
      case e: Concrete.Expr ⇒
        Argument.PositionArg(abstractify(e), Some(SourceInfo(e)))
      case x ⇒
        throw new RuntimeException("Unexpected argument: " + x)
    }
    Abstract.InvocationExpr(abstractify(function), abstractArgs, Some(SourceInfo(invocationExpr)))
  }

  private def abstractifyChainedComparision(chainedExpr: Concrete.ChainedOpExpr): Abstract.Expr = {
    val Concrete.ChainedOpExpr(left, opRights) = chainedExpr
    Abstract.ChainedOpExpr(abstractify(left), opRights.map {
      case (opToken, right) ⇒ (getBinaryOperator(opToken), abstractify(right))
    }, Some(SourceInfo(chainedExpr)))
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

}