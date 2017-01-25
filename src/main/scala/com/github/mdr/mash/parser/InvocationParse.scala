package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

import scala.collection.mutable.{ ArrayBuffer, ListBuffer }

trait InvocationParse { self: MashParse ⇒

  protected def invocationExpr(): Expr = {
    val expr = prefixExpr()
    val args = ListBuffer[AstNode]()
    safeWhile(!(PIPE || RPAREN || EOF || LONG_EQUALS || NOT_EQUALS || GREATER_THAN || GREATER_THAN_EQUALS || LESS_THAN ||
      LESS_THAN_EQUALS || AND || OR || PLUS || MINUS || TIMES || DIVIDE || IF || THEN || ELSE || SEMI || COMMA ||
      RSQUARE || ERROR || RBRACE || COLON || RIGHT_ARROW || SHORT_EQUALS || PLUS_EQUALS || MINUS_EQUALS || TIMES_EQUALS
      || DIVIDE_EQUALS || TILDE || DEF || CLASS || STRING_END || ELLIPSIS || LAZY)) {
      args += arg()
    }
    if (args.isEmpty)
      expr
    else
      InvocationExpr(expr, args)
  }

  private def longArg(): AstNode = {
    val flagToken = nextToken()
    if (SHORT_EQUALS) {
      val equalsToken = nextToken()
      val flagValue = prefixExpr()
      LongArg(flagToken, Some(equalsToken, flagValue))
    } else
      LongArg(flagToken)
  }

  private def shortArg(): AstNode = {
    val flagToken = nextToken()
    ShortArg(flagToken)
  }

  private def arg(): AstNode =
    if (SHORT_FLAG)
      shortArg()
    else if (LONG_FLAG)
      longArg()
    else
      prefixExpr()

  protected def parenInvocationExpr(previousExpr: Expr): ParenInvocationExpr = {
    val lparen = nextToken()
    if (RPAREN) {
      val rparen = nextToken()
      ParenInvocationExpr(previousExpr, lparen, None, rparen)
    } else {
      val firstArg = pipeExpr()
      val args = ArrayBuffer[(Token, Expr)]()
      safeWhile(COMMA) {
        val comma = nextToken()
        val arg = pipeExpr()
        args += (comma -> arg)
      }
      val rparen = consumeRequiredToken("invocation", RPAREN)
      ParenInvocationExpr(previousExpr, lparen, Some(ParenInvocationArgs(firstArg, args)), rparen)
    }
  }

}