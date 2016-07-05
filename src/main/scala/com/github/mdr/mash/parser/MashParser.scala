package com.github.mdr.mash.parser

import scala.PartialFunction.cond
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.parser.ConcreteSyntax._
import com.github.mdr.mash.utils.PointedRegion

case class ParseError(message: String, location: PointedRegion)

object MashParser {

  def parse(s: String, mish: Boolean = false): Either[ParseError, Expr] =
    try
      Right(doParse(s, forgiving = false, mish = mish))
    catch {
      case e: MashParserException ⇒ Left(e.parseError)
    }

  def parseForgiving(s: String, mish: Boolean = false): Expr =
    doParse(s, forgiving = true, mish = mish)

  private def doParse(s: String, forgiving: Boolean = true, mish: Boolean = false): Expr = {
    val tokens = MashLexer.tokenise(s, forgiving = forgiving, mish = mish, includeCommentsAndWhitespace = false).toArray
    val parse = new MashParse(tokens, forgiving = forgiving)
    if (mish)
      parse.mishExpr()
    else
      parse.program()
  }

  def parseExpr(s: String, forgiving: Boolean = true, mish: Boolean = false): Expr = {
    val tokens = MashLexer.tokenise(s, forgiving = forgiving, mish = mish).toArray
    val parse = new MashParse(tokens, forgiving = forgiving)
    if (mish)
      parse.mishExpr()
    else
      parse.expr()
  }

}

class MashParse(tokens: Array[Token], forgiving: Boolean = true) {

  import ConcreteSyntax._
  import TokenType._

  /**
   * Index of the token currently being examined
   */
  private var pos = 0

  /**
   * Return the token at the given position. If it's past the end of the token sequence, then return the last token (EOF).
   */
  private def token(pos: Int): Token =
    if (pos < tokens.length)
      tokens(pos)
    else
      tokens.last

  private def currentToken: Token = token(pos)

  private def currentTokenType = currentToken.tokenType

  private def currentPos = currentToken.offset

  private def currentLocation = PointedRegion(currentToken.offset, currentToken.region)

  /**
   *  Consume the current token, and advance to the next.
   *  @return the token before advancing
   */
  private def nextToken(): Token = {
    val token = currentToken
    pos += 1
    token
  }

  /**
   * Return the type of the token n positions in the stream ahead. If past the end, this will return EOF.
   */
  private def lookahead(n: Int): TokenType = token(pos + n).tokenType

  /**
   * We're testing token types a lot, a bit of shorthand helps.
   */
  private implicit def tokenType2Boolean(tokenType: TokenType): Boolean = currentTokenType == tokenType

  private def errorExpectedToken(expected: String) =
    throw new MashParserException(s"Expected '$expected', but instead found '${currentToken.text}'", currentLocation)

  def program(): Expr = {
    val result = statementSeq()
    if (!EOF && !forgiving)
      errorExpectedToken("end of input")
    result
  }

  def mishExpr(): MishExpr = {
    val command = mishItem()
    val args = ArrayBuffer[MishItem]()
    while (MISH_WORD || STRING_LITERAL || STRING_START || STRING_INTERPOLATION_START_SIMPLE || STRING_INTERPOLATION_START_COMPLEX)
      args += mishItem()
    MishExpr(command, args)
  }

  private def mishItem(): MishItem = {
    if (MISH_WORD)
      MishWord(nextToken())
    else if (STRING_LITERAL)
      MishString(Literal(nextToken()))
    else if (STRING_START)
      MishString(interpolatedString())
    else if (STRING_INTERPOLATION_START_SIMPLE || STRING_INTERPOLATION_START_COMPLEX)
      MishInterpolation(interpolationPart())
    else if (forgiving)
      MishWord(syntheticToken(MISH_WORD))
    else
      unexpectedToken()
  }

  def expr(): Expr = statementExpr()

  private def statementExpr(): Expr =
    if (DEF)
      functionDeclaration()
    else
      pipeExpr()

  private def pipeExpr(): Expr = {
    val expr = lambdaExpr(mayContainPipe = true)
    pipeExpr_(expr)
  }

  private def pipeExpr_(previousExpr: Expr): Expr =
    if (PIPE) {
      val pipeToken = nextToken()
      val right = lambdaExpr(mayContainPipe = false)
      val pipeExpr = PipeExpr(previousExpr, pipeToken, right)
      pipeExpr_(pipeExpr)
    } else
      previousExpr

  private def lambdaExpr(mayContainPipe: Boolean = false): Expr =
    if (IDENTIFIER && lookahead(1) == RIGHT_ARROW) {
      val param = nextToken()
      val arrow = nextToken()
      val body = if (mayContainPipe) pipeExpr() else lambdaExpr(mayContainPipe = false)
      LambdaExpr(param, arrow, body)
    } else
      assignmentExpr()

  private def assignmentExpr(): Expr = {
    val left = ifExpr()
    if (SHORT_EQUALS || PLUS_EQUALS || MINUS_EQUALS || TIMES_EQUALS || DIVIDE_EQUALS) {
      val equals = nextToken()
      val aliasOpt = if (ALIAS) Some(nextToken()) else None
      val right = pipeExpr()
      AssignmentExpr(left, equals, aliasOpt, right)
    } else
      left
  }

  private def ifExpr(): Expr =
    if (IF) {
      val ifToken = nextToken()
      val cond = orExpr()
      val thenToken =
        if (THEN)
          nextToken()
        else if (forgiving) {
          val lastTokenOfCond = cond.tokens.last
          Token(THEN, lastTokenOfCond.region.posAfter, 0, lastTokenOfCond.source)
        } else
          errorExpectedToken("then")
      val body = pipeExpr()
      val elseOpt =
        if (ELSE) {
          val elseToken = nextToken()
          val elseBody = pipeExpr()
          Some(elseToken, elseBody)
        } else
          None
      IfExpr(ifToken, cond, thenToken, body, elseOpt)
    } else
      orExpr()

  private def orExpr(): Expr = {
    val expr = andExpr()
    if (OR) {
      val or = nextToken()
      val right = orExpr()
      BinOpExpr(expr, or, right)
    } else
      expr
  }

  private def andExpr(): Expr = {
    val expr = comparisonExpr()
    if (AND) {
      val and = nextToken()
      val right = andExpr()
      BinOpExpr(expr, and, right)
    } else
      expr
  }

  private def continueChaining(op: Token) = cond(op.tokenType) {
    case LESS_THAN | LESS_THAN_EQUALS       ⇒ LESS_THAN || LESS_THAN_EQUALS
    case GREATER_THAN | GREATER_THAN_EQUALS ⇒ GREATER_THAN || GREATER_THAN_EQUALS
  }

  private def comparisonExpr(): Expr = {
    val expr = additiveExpr()
    if (LONG_EQUALS || NOT_EQUALS || GREATER_THAN || GREATER_THAN_EQUALS || LESS_THAN_EQUALS || LESS_THAN) {
      val op = nextToken()
      val right = additiveExpr()
      if (continueChaining(op)) {
        val opExprs = ArrayBuffer(op -> right)
        while (continueChaining(op)) {
          val op2 = nextToken()
          val right2 = additiveExpr()
          opExprs += (op2 -> right2)
        }
        ChainedOpExpr(expr, opExprs)
      } else
        BinOpExpr(expr, op, right)
    } else
      expr
  }

  private def additiveExpr(): Expr = {
    var expr = multiplicativeExpr()
    while (PLUS | MINUS) {
      val op = nextToken()
      val right = multiplicativeExpr()
      expr = BinOpExpr(expr, op, right)
    }
    expr
  }

  private def multiplicativeExpr(): Expr = {
    var expr = invocationExpr()
    while (TIMES | DIVIDE) {
      val op = nextToken()
      val right = invocationExpr()
      expr = BinOpExpr(expr, op, right)
    }
    expr
  }

  private def invocationExpr(): Expr = {
    val expr = prefixExpr()
    val args = ListBuffer[AstNode]()
    var previousPos = pos
    while (!(PIPE || RPAREN || EOF || LONG_EQUALS || NOT_EQUALS || GREATER_THAN || GREATER_THAN_EQUALS || LESS_THAN ||
      LESS_THAN_EQUALS || AND || OR || PLUS || MINUS || TIMES || DIVIDE || IF || THEN || ELSE || SEMI || COMMA ||
      RSQUARE || ERROR || RBRACE || COLON || RIGHT_ARROW || SHORT_EQUALS || PLUS_EQUALS || MINUS_EQUALS || TIMES_EQUALS
      || DIVIDE_EQUALS || TILDE || DEF || STRING_END || ALIAS || ELLIPSIS)) {
      args += arg()
      assert(pos > previousPos, "Infinite loop detected parsing invocationExpr at position " + pos + ", current token is " + currentToken)
      previousPos = pos
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

  private def prefixExpr(): Expr =
    if (MINUS) {
      val minus = nextToken()
      val expr = prefixExpr()
      MinusExpr(minus, expr)
    } else if (DOT || DOT_NULL_SAFE) {
      val dotToken = nextToken()
      val identifier =
        if (IDENTIFIER)
          nextToken()
        else if (forgiving)
          syntheticToken(IDENTIFIER, dotToken)
        else
          errorExpectedToken("identifier")
      val expr: Expr = HeadlessMemberExpr(dotToken, identifier)
      suffixExpr_(expr)
    } else
      suffixExpr()

  private def suffixExpr(): Expr = {
    val expr = primaryExpr()
    suffixExpr_(expr)
  }

  private def suffixExpr_(previousExpr: Expr): Expr =
    if (DOT || DOT_NULL_SAFE)
      suffixExpr_(memberExpr(previousExpr))
    else if (LSQUARE_LOOKUP)
      suffixExpr_(lookupExpr(previousExpr))
    else if (LPAREN_INVOKE)
      suffixExpr_(parenInvocationExpr(previousExpr))
    else if (QUESTION)
      suffixExpr_(helpExpr(previousExpr))
    else
      previousExpr

  private def helpExpr(previousExpr: Expr): HelpExpr = {
    val question = nextToken()
    HelpExpr(previousExpr, question)
  }

  private def memberExpr(previousExpr: Expr): MemberExpr = {
    val dotToken = nextToken()
    val identifier =
      if (IDENTIFIER)
        nextToken()
      else if (forgiving)
        syntheticToken(IDENTIFIER, dotToken)
      else
        errorExpectedToken("identifier")
    MemberExpr(previousExpr, dotToken, identifier)
  }

  private def lookupExpr(previousExpr: Expr): LookupExpr = {
    val lsquare = nextToken()
    val indexExpr = pipeExpr()
    val rsquare =
      if (RSQUARE)
        nextToken()
      else if (forgiving)
        syntheticToken(RSQUARE, indexExpr.tokens.last)
      else
        errorExpectedToken("]")
    LookupExpr(previousExpr, lsquare, indexExpr, rsquare)
  }

  private def parenInvocationExpr(previousExpr: Expr): ParenInvocationExpr = {
    val lparen = nextToken()
    if (RPAREN) {
      val rparen = nextToken()
      ParenInvocationExpr(previousExpr, lparen, None, rparen)
    } else {
      val firstArg = pipeExpr()
      val args = ArrayBuffer[(Token, Expr)]()
      while (COMMA) {
        val comma = nextToken()
        val arg = pipeExpr()
        args += (comma -> arg)
      }
      val rparen =
        if (RPAREN)
          nextToken()
        else if (forgiving) {
          val lastExpr = (firstArg +: args.map(_._2)).last
          val lastToken = lastExpr.tokens.last
          syntheticToken(RPAREN, lastToken)
        } else
          errorExpectedToken(")")
      ParenInvocationExpr(previousExpr, lparen, Some(ParenInvocationArgs(firstArg, args)), rparen)
    }
  }

  private def complexInterpolation(): ComplexInterpolation = {
    val interpolationStart = nextToken()
    val interpolatedExpr = pipeExpr()
    val rbrace =
      if (RBRACE)
        nextToken()
      else if (forgiving)
        syntheticToken(RBRACE, expr.tokens.last)
      else
        errorExpectedToken("}")
    ComplexInterpolation(interpolationStart, interpolatedExpr, rbrace)
  }

  private def simpleInterpolation(): SimpleInterpolation = {
    val start = nextToken()
    var expr: Expr =
      if (HOLE)
        Hole(nextToken())
      else if (IDENTIFIER)
        Identifier(nextToken())
      else
        errorExpectedToken("identifier or _") // shouldn't happen
    while (DOT) {
      val dot = nextToken()
      val ident =
        if (IDENTIFIER)
          nextToken()
        else if (forgiving)
          syntheticToken(IDENTIFIER, dot)
        else
          errorExpectedToken("identifier")
      expr = MemberExpr(expr, dot, ident)
    }
    SimpleInterpolation(start, expr)
  }

  private def interpolationPart(): InterpolationPart =
    if (STRING_MIDDLE)
      StringPart(nextToken())
    else if (STRING_INTERPOLATION_START_COMPLEX)
      complexInterpolation()
    else
      simpleInterpolation()

  private def interpolatedString(): InterpolatedString = {
    val stringStart = nextToken()
    val parts = ArrayBuffer[InterpolationPart]()
    while (STRING_MIDDLE || STRING_INTERPOLATION_START_COMPLEX || STRING_INTERPOLATION_START_SIMPLE)
      parts += interpolationPart()
    val end =
      if (STRING_END)
        nextToken()
      else if (forgiving)
        syntheticToken(STRING_END)
      else
        errorExpectedToken("end of string")
    InterpolatedString(stringStart, parts, end)
  }

  private def primaryExpr(): Expr =
    if (NUMBER_LITERAL || STRING_LITERAL || TRUE || FALSE || NULL)
      Literal(nextToken())
    else if (STRING_START)
      interpolatedString()
    else if (IDENTIFIER)
      Identifier(nextToken())
    else if (MISH_WORD)
      MishFunction(nextToken())
    else if (HOLE)
      Hole(nextToken())
    else if (LPAREN)
      parenExpr()
    else if (LSQUARE)
      listExpr()
    else if (LBRACE) {
      if (lookahead(1).isIdentifier && lookahead(2) == COLON || lookahead(1) == RBRACE)
        objectExpr()
      else
        blockExpr()
    } else if (MISH_INTERPOLATION_START || MISH_INTERPOLATION_START_NO_CAPTURE)
      mishInterpolation()
    else if (forgiving)
      Literal(syntheticToken(STRING_LITERAL))
    else
      unexpectedToken()

  private def unexpectedToken() =
    if (EOF)
      throw new MashParserException(s"Unexpected end-of-input", currentLocation)
    else
      throw new MashParserException(s"Unexpected token '${currentToken.text}'", currentLocation)

  private def statementSeq(): Expr = {
    var statements = ArrayBuffer[Statement]()
    var continue = true
    while (continue) {
      if (RBRACE || RPAREN || EOF)
        continue = false
      else if (SEMI) {
        val semi = nextToken()
        statements += Statement(statementOpt = None, Some(semi))
      } else {
        val statement = statementExpr()
        if (SEMI) {
          val semi = nextToken()
          statements += Statement(Some(statement), Some(semi))
        } else {
          statements += Statement(Some(statement), semiOpt = None)
          continue = false
        }
      }
    }
    statements match {
      case Seq(Statement(Some(statement), None)) ⇒ statement
      case _                                     ⇒ StatementSeq(statements)
    }
  }

  private def blockExpr(): BlockExpr = {
    val lbrace = nextToken()
    val statements = statementSeq()
    val rbrace =
      if (RBRACE)
        nextToken()
      else if (forgiving)
        syntheticToken(RBRACE)
      else
        errorExpectedToken("}")
    BlockExpr(lbrace, statements, rbrace)
  }

  private def mishInterpolation(): MishInterpolationExpr = {
    val start = nextToken()
    val expr = mishExpr()
    val rbrace =
      if (RBRACE)
        nextToken()
      else if (forgiving)
        syntheticToken(RBRACE, expr.tokens.last)
      else
        errorExpectedToken("}")
    MishInterpolationExpr(start, expr, rbrace)
  }

  private def parenExpr(): Expr = {
    val lparen = nextToken()
    val expr = statementSeq()
    val rparen =
      if (RPAREN)
        nextToken()
      else if (forgiving)
        syntheticToken(RPAREN, expr.tokens.lastOption.getOrElse(lparen))
      else
        errorExpectedToken(")")
    ParenExpr(lparen, expr, rparen)
  }

  private def listExpr(): Expr = {
    val lsquare = nextToken()
    if (RSQUARE) {
      val rsquare = nextToken()
      ListExpr(lsquare, None, rsquare)
    } else {
      val firstItem = pipeExpr()
      val items = ArrayBuffer[(Token, Expr)]()
      while (COMMA) {
        val comma = nextToken()
        val item = pipeExpr()
        items += (comma -> item)
      }
      val rsquare =
        if (RSQUARE)
          nextToken()
        else if (forgiving) {
          val lastExpr = (firstItem +: items.map(_._2)).last
          val lastToken = lastExpr.tokens.last
          syntheticToken(RSQUARE, lastToken)
        } else
          errorExpectedToken("]")
      ListExpr(lsquare, Some(ListExprContents(firstItem, items)), rsquare)
    }
  }

  private def objectEntry(): ObjectEntry = {
    val label =
      if (IDENTIFIER)
        nextToken()
      else if (forgiving)
        syntheticToken(IDENTIFIER)
      else
        errorExpectedToken("identifier")
    val colon =
      if (COLON)
        nextToken()
      else if (forgiving)
        syntheticToken(COLON)
      else
        throw errorExpectedToken(":")
    val expr = pipeExpr()
    ObjectEntry(label, colon, expr)
  }

  private def objectExpr(): Expr = {
    val lbrace = nextToken()
    if (RBRACE) {
      val rbrace = nextToken()
      ObjectExpr(lbrace, None, rbrace)
    } else {
      val firstEntry = objectEntry()
      val entries = ArrayBuffer[(Token, ObjectEntry)]()
      var continue = true
      while (COMMA) {
        val comma = nextToken()
        val entry = objectEntry()
        entries += (comma -> entry)
      }
      val rbrace =
        if (RBRACE)
          nextToken()
        else if (forgiving) {
          val lastExpr = (firstEntry +: entries.map(_._2)).last
          val lastToken = lastExpr.tokens.last
          syntheticToken(RBRACE, lastToken)
        } else
          errorExpectedToken("}")
      ObjectExpr(lbrace, Some(ObjectExprContents(firstEntry, entries)), rbrace)
    }
  }

  private def functionDeclaration(): FunctionDeclaration = {
    val defToken = nextToken()
    val name =
      if (IDENTIFIER)
        nextToken()
      else if (forgiving)
        syntheticToken(IDENTIFIER, defToken)
      else
        errorExpectedToken("identifier")
    val params = ArrayBuffer[FunctionParam]()
    while (IDENTIFIER) {
      val ident = nextToken()
      val param =
        if (ELLIPSIS) {
          val ellipsis = nextToken()
          VariadicParam(ident, ellipsis)
        } else
          SimpleParam(ident)
      params += param
    }
    val equals =
      if (SHORT_EQUALS)
        nextToken()
      else if (forgiving)
        syntheticToken(SHORT_EQUALS, params.lastOption.map(_.tokens.last).getOrElse(name))
      else
        errorExpectedToken("=")
    val body = pipeExpr()
    FunctionDeclaration(defToken, name, params, equals, body)
  }

  private def syntheticToken(tokenType: TokenType): Token =
    syntheticToken(tokenType, afterTokenOpt = None)

  private def syntheticToken(tokenType: TokenType, afterToken: Token): Token =
    syntheticToken(tokenType, Some(afterToken))

  private def syntheticToken(tokenType: TokenType, afterTokenOpt: Option[Token]): Token =
    Token(tokenType, afterTokenOpt.map(_.region.posAfter).getOrElse(0), 0, afterTokenOpt.map(_.source).getOrElse(""))

}