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
import com.github.mdr.mash.lexer.LexerResult

class MashParse(lexerResult: LexerResult, initialForgiving: Boolean) extends Parse(lexerResult, initialForgiving) {

  import ConcreteSyntax._
  import TokenType._

  def program(): Expr = {
    val result = statementSeq()
    if (!EOF && !forgiving)
      errorExpectedToken("end of input")
    result
  }

  def mishExpr(): MishExpr = {
    val command = mishItem()
    val args = ArrayBuffer[MishItem]()
    safeWhile(MISH_WORD || STRING_LITERAL || STRING_START || STRING_INTERPOLATION_START_SIMPLE || STRING_INTERPOLATION_START_COMPLEX) {
      args += mishItem()
    }
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

  case class LambdaStart(paramList: ParamList, arrow: Token)

  private def lambdaStart(): LambdaStart = {
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

  private def lambdaExpr(mayContainPipe: Boolean = false): Expr = speculate(lambdaStart()) match {
    case Some(LambdaStart(params, arrow)) ⇒
      val body = if (mayContainPipe) pipeExpr() else lambdaExpr(mayContainPipe = false)
      LambdaExpr(params, arrow, body)
    case None ⇒
      assignmentExpr()
  }

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
        safeWhile(continueChaining(op)) {
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
    safeWhile(PLUS | MINUS) {
      val op = nextToken()
      val right = multiplicativeExpr()
      expr = BinOpExpr(expr, op, right)
    }
    expr
  }

  private def multiplicativeExpr(): Expr = {
    var expr = invocationExpr()
    safeWhile(TIMES | DIVIDE) {
      val op = nextToken()
      val right = invocationExpr()
      expr = BinOpExpr(expr, op, right)
    }
    expr
  }

  private def invocationExpr(): Expr = {
    val expr = prefixExpr()
    val args = ListBuffer[AstNode]()
    safeWhile(!(PIPE || RPAREN || EOF || LONG_EQUALS || NOT_EQUALS || GREATER_THAN || GREATER_THAN_EQUALS || LESS_THAN ||
      LESS_THAN_EQUALS || AND || OR || PLUS || MINUS || TIMES || DIVIDE || IF || THEN || ELSE || SEMI || COMMA ||
      RSQUARE || ERROR || RBRACE || COLON || RIGHT_ARROW || SHORT_EQUALS || PLUS_EQUALS || MINUS_EQUALS || TIMES_EQUALS
      || DIVIDE_EQUALS || TILDE || DEF || STRING_END || ALIAS || ELLIPSIS)) {
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
      safeWhile(COMMA) {
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
    safeWhile(DOT) {
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
    safeWhile(STRING_MIDDLE || STRING_INTERPOLATION_START_COMPLEX || STRING_INTERPOLATION_START_SIMPLE) {
      parts += interpolationPart()
    }
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
    else if (LBRACE)
      speculate(objectExpr()) getOrElse blockExpr()
    else if (MISH_INTERPOLATION_START || MISH_INTERPOLATION_START_NO_CAPTURE)
      mishInterpolation()
    else if (forgiving)
      Literal(syntheticToken(STRING_LITERAL))
    else
      unexpectedToken()

  private def statementSeq(): Expr = {
    var statements = ArrayBuffer[Statement]()
    var continue = true
    safeWhile(continue) {
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
      safeWhile(COMMA) {
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
        errorExpectedToken(":")
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
      safeWhile(COMMA) {
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
    safeWhile(IDENTIFIER || LPAREN) {
      params += parameter()
    }
    ParamList(params)
  }

  private def parameter(): FunctionParam =
    if (IDENTIFIER) {
      val ident = nextToken()
      if (ELLIPSIS) {
        val ellipsis = nextToken()
        VariadicParam(ident, ellipsis)
      } else
        SimpleParam(ident)
    } else if (LPAREN) {
      val lparen = nextToken()
      val param = parameter()
      val rparen =
        if (RPAREN)
          nextToken()
        else if (forgiving)
          syntheticToken(RPAREN)
        else
          errorExpectedToken(")")
      ParenParam(lparen, param, rparen)
    } else if (forgiving)
      SimpleParam(syntheticToken(IDENTIFIER))
    else
      errorExpectedToken("identifier")

}
