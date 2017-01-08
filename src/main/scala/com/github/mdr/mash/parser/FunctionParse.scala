package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

import scala.collection.mutable.ArrayBuffer

trait FunctionParse {
  self: MashParse ⇒

  protected def functionDeclaration(): FunctionDeclaration = {
    val defToken = nextToken()
    val name =
      if (IDENTIFIER)
        nextToken()
      else if (forgiving)
        syntheticToken(IDENTIFIER, defToken)
      else
        errorExpectedToken("identifier")
    val params = paramList()
    val equals =
      if (SHORT_EQUALS)
        nextToken()
      else if (forgiving)
        syntheticToken(SHORT_EQUALS, params.params.lastOption.map(_.tokens.last).getOrElse(name))
      else
        errorExpectedToken("=")
    val body = pipeExpr()
    FunctionDeclaration(defToken, name, params, equals, body)
  }

  private def paramList(): ParamList = {
    val params = ArrayBuffer[FunctionParam]()
    safeWhile(IDENTIFIER || LPAREN || LBRACE || HOLE) {
      params += parameter()
    }
    ParamList(params)
  }

  private def parameter(parenAllowed: Boolean = true): FunctionParam =
    if (HOLE) {
      val hole = nextToken()
      PatternParam(HolePattern(hole))
    } else if (IDENTIFIER) {
      val ident = nextToken()
      if (ELLIPSIS) {
        val ellipsis = nextToken()
        VariadicParam(ident, ellipsis)
      } else
        SimpleParam(ident)
    } else if (LPAREN && parenAllowed) {
      val lparen = nextToken()
      val lazyOpt = if (LAZY) Some(nextToken()) else None
      val param = parameter(parenAllowed = false)
      val actualParam =
        param match {
          case SimpleParam(name) ⇒
            if (SHORT_EQUALS && param.isInstanceOf[SimpleParam]) {
              val equals = nextToken()
              val defaultExpr = pipeExpr()
              DefaultParam(name, equals, defaultExpr)
            } else
              param
          case _                 ⇒
            param
        }
      val rparen =
        if (RPAREN)
          nextToken()
        else if (forgiving)
          syntheticToken(RPAREN)
        else
          errorExpectedToken(")")
      ParenParam(lparen, lazyOpt, actualParam, rparen)
    } else if (LBRACE)
      PatternParam(pattern())
    else if (forgiving)
      SimpleParam(syntheticToken(IDENTIFIER))
    else
      errorExpectedToken("identifier")

  private def objectPattern(): ObjectPattern = {
    val lbrace = nextToken()
    if (RBRACE) {
      val rbrace = nextToken()
      ObjectPattern(lbrace, None, rbrace)
    } else {
      def parseEntry(): ObjectPatternEntry = {
        if (IDENTIFIER) {
          val field = nextToken()
          if (COLON) {
            val colon = nextToken()
            val valuePattern = pattern()
            FullObjectPatternEntry(field, colon, valuePattern)
          } else
            ShorthandObjectPatternEntry(field)
        }
        else
          ShorthandObjectPatternEntry(syntheticToken(IDENTIFIER))
      }
      val firstEntry = parseEntry()
      val entries = ArrayBuffer[(Token, ObjectPatternEntry)]()
      safeWhile(COMMA) {
        val comma = nextToken()
        val entry = parseEntry()
        entries += (comma -> entry)
      }
      val rbrace =
        if (RBRACE)
          nextToken()
        else if (forgiving)
          syntheticToken(RBRACE)
        else
          errorExpectedToken("]")
      ObjectPattern(lbrace, Some(ObjectPatternContents(firstEntry, entries)), rbrace)
    }
  }

  protected def pattern(): Pattern =
    if (HOLE)
      HolePattern(nextToken())
    else if (IDENTIFIER)
      IdentPattern(nextToken())
    else
      objectPattern()

  protected case class LambdaStart(paramList: ParamList, arrow: Token)

  protected def lambdaStart(): LambdaStart = {
    val params = paramList()
    val arrow =
      if (RIGHT_ARROW)
        nextToken()
      else if (forgiving)
        syntheticToken(RIGHT_ARROW)
      else
        errorExpectedToken("=>")
    LambdaStart(params, arrow)
  }

  protected def completeLambdaExpr(lambdaStart: LambdaStart, mayContainPipe: Boolean = false, mayContainStatementSeq: Boolean = false): Expr = {
    val body =
      if (mayContainStatementSeq) statementSeq()
      else if (mayContainPipe) pipeExpr()
      else lambdaExpr(mayContainStatementSeq = false, mayContainPipe = false)
    LambdaExpr(lambdaStart.paramList, lambdaStart.arrow, body)
  }

  protected def lambdaExpr(mayContainPipe: Boolean = false, mayContainStatementSeq: Boolean = false): Expr =
    speculate(lambdaStart()) match {
      case Some(lambdaStart) ⇒
        completeLambdaExpr(lambdaStart, mayContainPipe = mayContainPipe, mayContainStatementSeq = mayContainStatementSeq)
      case None              ⇒
        patternAssignmentExpr()
    }
}