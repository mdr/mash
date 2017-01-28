package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

import scala.collection.mutable.ArrayBuffer

trait FunctionParse {
  self: MashParse ⇒

  private def attribute(): Attribute = {
    val atToken = nextToken()
    val name = consumeRequiredToken("attribute", IDENTIFIER)
    Attribute(atToken, name)
  }

  private def attributes(): Attributes = {
    val attributes = safeWhile(AT) { attribute() }
    Attributes(attributes)
  }

  protected def functionDeclaration(): FunctionDeclaration = {
    val attributesOpt = if (AT) Some(attributes()) else None
    val defToken = consumeRequiredToken("function", DEF)
    val name = consumeRequiredToken("function", IDENTIFIER)
    val params = paramList()
    val equals = consumeRequiredToken(s"function '${name.text}'", SHORT_EQUALS)
    val body = pipeExpr()
    FunctionDeclaration(attributesOpt, docComment(defToken), defToken, name, params, equals, body)
  }

  protected def paramList(): ParamList = {
    val params = ArrayBuffer[Param]()
    safeWhile(IDENTIFIER || LPAREN || LBRACE || LSQUARE || HOLE) {
      params += parameter()
    }
    ParamList(params)
  }

  protected def classParamList(): ParamList = {
    val params = ArrayBuffer[Param]()
    safeWhile(IDENTIFIER || LPAREN || LSQUARE || HOLE) {
      params += parameter()
    }
    ParamList(params)
  }

  protected def parameter(withinParen: Boolean = false): Param =
    if (IDENTIFIER) {
      val ident = nextToken()
      if (ELLIPSIS) {
        val ellipsis = nextToken()
        VariadicParam(ident, ellipsis)
      } else
        SimpleParam(ident)
    } else if (LPAREN && !withinParen) {
      val lparen = nextToken()
      val lazyOpt = if (LAZY) Some(nextToken()) else None
      val param = parameter(withinParen = true)
      val actualParam =
        param match {
          case SimpleParam(name) ⇒
            if (SHORT_EQUALS) {
              val equals = nextToken()
              val defaultExpr = pipeExpr()
              DefaultParam(IdentPattern(name), equals, defaultExpr)
            } else
              param
          case _                 ⇒
            param
        }
      val rparen = consumeRequiredToken("parameter", RPAREN)
      ParenParam(lparen, lazyOpt, actualParam, rparen)
    } else if (LBRACE || LSQUARE || HOLE) {
      val pat = pattern()
      if (SHORT_EQUALS && withinParen) {
        val equals = nextToken()
        val defaultExpr = pipeExpr()
        DefaultParam(pat, equals, defaultExpr)
      } else
        PatternParam(pat)
    }
    else if (forgiving)
      SimpleParam(syntheticToken(IDENTIFIER))
    else
      errorExpectedToken(Some("parameter"), "identifier")

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
      val rbrace = consumeRequiredToken("object pattern", RBRACE)
      ObjectPattern(lbrace, Some(ObjectPatternContents(firstEntry, entries)), rbrace)
    }
  }

  protected def pattern(): Pattern =
    if (HOLE)
      HolePattern(nextToken())
    else if (IDENTIFIER)
      IdentPattern(nextToken())
    else if (LSQUARE)
      listPattern()
    else
      objectPattern()

  private def listPattern(): Pattern = {
    val lsquare = nextToken()
    if (RSQUARE) {
      val rsquare = nextToken()
      ListPattern(lsquare, None, rsquare)
    } else {
      val firstElement = pattern()
      val otherElements = ArrayBuffer[(Token, Pattern)]()
      safeWhile(COMMA) {
        val comma = nextToken()
        val item = pattern()
        otherElements += (comma -> item)
      }
      val rsquare = consumeRequiredToken("list pattern", RSQUARE)
      ListPattern(lsquare, Some(ListPatternContents(firstElement, otherElements)), rsquare)
    }
  }

  protected case class LambdaStart(paramList: ParamList, arrow: Token)

  protected def lambdaStart(): LambdaStart = {
    val params = paramList()
    val arrow = consumeRequiredToken("lambda", RIGHT_ARROW)
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