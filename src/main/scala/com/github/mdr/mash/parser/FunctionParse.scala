package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

trait FunctionParse {
  self: MashParse ⇒

  private def attribute(): Attribute = {
    val atToken = nextToken()
    val name = consumeRequiredToken("attribute", IDENTIFIER)
    Attribute(atToken, name)
  }

  private def attributes() = Attributes(safeWhile(AT)(attribute()))

  protected def functionDeclaration(): FunctionDeclaration = {
    val firstToken = currentToken
    val (attributesOpt, defToken, name, params, equals) = noSemis {
      val attributesOpt = if (AT) Some(attributes()) else None
      val defToken = consumeRequiredToken("function", DEF)
      val name = consumeRequiredToken("function", IDENTIFIER)
      val params = paramList()
      val equals = consumeRequiredToken(s"function '${name.text}'", SHORT_EQUALS)
      (attributesOpt, defToken, name, params, equals)
    }
    val body = pipeExpr()
    FunctionDeclaration(docComment(firstToken), attributesOpt, defToken, name, params, equals, body)
  }

  protected def paramList(): ParamList = {
    def isParamStart = IDENTIFIER || LPAREN || LBRACE || LSQUARE || HOLE
    val params =
      if (isParamStart)
        parameter() +: noSemis(safeWhile(isParamStart)(parameter()))
      else
        Seq()
    ParamList(params)
  }

  private def parenParam(): ParenParam = {
    val lparen = consumeRequiredToken("parameter", LPAREN)
    val attributesOpt = if (AT) Some(attributes()) else None
    val pat = pattern()
    val ellipsisOpt = consumeOptionalToken(ELLIPSIS)
    val childParam = SimpleParam(pat, ellipsisOpt)
    val equalsDefaultOpt =
      if (SHORT_EQUALS) {
        val equals = nextToken()
        val defaultExpr = pipeExpr()
        Some((equals, defaultExpr))
      } else
        None
    val rparen = consumeRequiredToken("parameter", RPAREN)
    ParenParam(lparen, attributesOpt, childParam, equalsDefaultOpt, rparen)
  }

  protected def parameter(): Param =
    if (IDENTIFIER) {
      val ident = nextToken()
      val ellipsisOpt = consumeOptionalToken(ELLIPSIS)
      SimpleParam(IdentPattern(ident), ellipsisOpt)
    } else if (LPAREN)
      parenParam()
    else if (LBRACE || LSQUARE || HOLE)
      SimpleParam(pattern())
    else {
      val ident = consumeRequiredToken("parameter", IDENTIFIER)
      SimpleParam(IdentPattern(ident))
    }

  private def objectPatternEntry(): ObjectPatternEntry = {
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

  private def objectPattern(): ObjectPattern = {
    val lbrace = nextToken()
    if (RBRACE) {
      val rbrace = nextToken()
      ObjectPattern(lbrace, None, rbrace)
    } else {
      val firstEntry = objectPatternEntry()
      val entries = safeWhile(COMMA) {
        val comma = nextToken()
        val entry = objectPatternEntry()
        comma -> entry
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
      val otherElements = safeWhile(COMMA) {
        val comma = nextToken()
        val item = pattern()
        comma -> item
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

  protected def completeLambdaExpr(lambdaStart: LambdaStart,
                                   mayContainPipe: Boolean = false,
                                   mayContainStatementSeq: Boolean = false): Expr = {
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