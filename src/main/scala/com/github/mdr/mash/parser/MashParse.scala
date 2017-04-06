package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.lexer.{ LexerResult, Token }
import com.github.mdr.mash.parser.ConcreteSyntax._

import scala.PartialFunction.cond
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

class MashParse(lexerResult: LexerResult, initialForgiving: Boolean)
  extends Parse(lexerResult, initialForgiving)
    with InterpolatedStringParse
    with ObjectParse
    with MishParse
    with FunctionParse
    with ClassParse
    with InvocationParse {

  def program(): Program = {
    val namespaceDeclarationOpt = if (NAMESPACE) Some(namespaceDeclaration()) else None
    val result = statementSeq()
    if (!EOF && !forgiving)
      errorExpectedToken(Some("program"), "end of input")
    Program(namespaceDeclarationOpt, result)
  }

  private def namespaceDeclaration(): NamespaceDeclaration = {
    val namespace = consumeRequiredToken("namespace", NAMESPACE)
    val firstSegment = consumeRequiredToken("namespace", IDENTIFIER)
    val dotSegments = safeWhile(DOT) {
      val dot = nextToken()
      val segment = consumeRequiredToken("namespace", IDENTIFIER)
      (dot, segment)
    }
    NamespaceDeclaration(namespace, firstSegment, dotSegments)
  }

  protected def statementExpr(): Expr =
    if (AT) {
      noSemis {
        val attrs = attributes()
        if (CLASS)
          classDeclaration(Some(attrs))
        else // DEF
          functionDeclaration(Some(attrs))
      }
    } else if (DEF)
      functionDeclaration()
    else if (CLASS)
      classDeclaration()
    else if (IMPORT)
      importStatement()
    else
      pipeExpr()

  protected def importStatement(): ImportStatement = {
    val importToken = nextToken()
    val firstIdent = consumeRequiredToken("import statement", IDENTIFIER)
    @tailrec
    def readImportStatement(exprSoFar: Expr): ImportStatement = {
      val dot = consumeRequiredToken("import statement", DOT)
      if (HOLE) {
        val hole = nextToken()
        ImportStatement(importToken, exprSoFar, dot, hole)
      } else if (IDENTIFIER && lookahead(1) != DOT) {
        val ident = nextToken()
        ImportStatement(importToken, exprSoFar, dot, ident)
      } else if (IDENTIFIER) {
        val ident = consumeRequiredToken("import statement", IDENTIFIER)
        readImportStatement(MemberExpr(exprSoFar, dot, ident))
      } else if (forgiving) {
        val ident = syntheticToken(IDENTIFIER)
        ImportStatement(importToken, exprSoFar, dot, ident)
      } else
        errorExpectedToken(Some("import statement"), describeToken(IDENTIFIER))
    }
    readImportStatement(Identifier(firstIdent))
  }

  protected def pipeExpr(): Expr = {
    val expr = lambdaExpr(mayContainPipe = true)
    continuePipeExpr(expr)
  }

  private def continuePipeExpr(previousExpr: Expr): Expr =
    if (PIPE) {
      val pipeToken = nextToken()
      val right = lambdaExpr(mayContainPipe = false)
      val pipeExpr = PipeExpr(previousExpr, pipeToken, right)
      continuePipeExpr(pipeExpr)
    } else
      previousExpr

  private def isPatternStart = HOLE || IDENTIFIER || LBRACE || LSQUARE

  protected def patternAssignmentExpr(): Expr = {
    if (!isPatternStart)
      actualAssignmentExpr()
    else {
      val patternEqualsOpt = speculate("patternEquals")(patternEquals())
      patternEqualsOpt match {
        case Some((pat, equals)) ⇒
          val right = pipeExpr()
          PatternAssignmentExpr(pat, equals, right)
        case None                ⇒
          actualAssignmentExpr()
      }
    }
  }

  private def patternEquals(): (Pattern, Token) = {
    val pat = pattern()
    val equals = consumeRequiredToken("assignment", SHORT_EQUALS)
    (pat, equals)
  }

  private def actualAssignmentExpr(): Expr = {
    val left = ifExpr()
    if (SHORT_EQUALS || PLUS_EQUALS || MINUS_EQUALS || TIMES_EQUALS || DIVIDE_EQUALS) {
      val equals = nextToken()
      val right = pipeExpr()
      AssignmentExpr(left, equals, right)
    } else
      left
  }

  private def ifExpr(): Expr =
    if (IF) {
      val ifToken = nextToken()
      val cond = orExpr()
      val thenToken = consumeRequiredToken("if expression", THEN)
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

  private def continueChaining(op: Token): Boolean = cond(op.tokenType) {
    case LESS_THAN | LESS_THAN_EQUALS       ⇒ LESS_THAN || LESS_THAN_EQUALS
    case GREATER_THAN | GREATER_THAN_EQUALS ⇒ GREATER_THAN || GREATER_THAN_EQUALS
  }

  private def comparisonExpr(): Expr = {
    val expr = additiveExpr()
    if (LONG_EQUALS || NOT_EQUALS || GREATER_THAN || GREATER_THAN_EQUALS || LESS_THAN_EQUALS || LESS_THAN) {
      val op = nextToken()
      val right = additiveExpr()
      if (continueChaining(op)) {
        val opExprs = Seq(op -> right) ++
          safeWhile(continueChaining(op)) {
            val op2 = nextToken()
            val right2 = additiveExpr()
            op2 -> right2
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

  protected def prefixExpr(): Expr =
    if (MINUS) {
      val minus = nextToken()
      val expr = prefixExpr()
      MinusExpr(minus, expr)
    } else if (DOT || DOT_NULL_SAFE) {
      val dotToken = nextToken()
      val identifier = consumeRequiredToken("member expression", IDENTIFIER)
      continueSuffixExpr(HeadlessMemberExpr(dotToken, identifier))
    } else
      suffixExpr()

  protected def suffixExpr(): Expr = {
    val expr = primaryExpr()
    continueSuffixExpr(expr)
  }

  private def continueSuffixExpr(previousExpr: Expr): Expr =
    if (DOT || DOT_NULL_SAFE)
      continueSuffixExpr(memberExpr(previousExpr))
    else if (LSQUARE_LOOKUP)
      continueSuffixExpr(lookupExpr(previousExpr))
    else if (LPAREN_INVOKE)
      continueSuffixExpr(parenInvocationExpr(previousExpr))
    else if (QUESTION)
      continueSuffixExpr(helpExpr(previousExpr))
    else
      previousExpr

  private def helpExpr(previousExpr: Expr): HelpExpr = {
    val question = nextToken()
    HelpExpr(previousExpr, question)
  }

  private def memberExpr(previousExpr: Expr): MemberExpr = {
    val dotToken = nextToken()
    val identifier = consumeRequiredToken("member expression", IDENTIFIER)
    MemberExpr(previousExpr, dotToken, identifier)
  }

  private def lookupExpr(previousExpr: Expr): LookupExpr = {
    val lsquare = nextToken()
    val indexExpr = pipeExpr()
    val rsquare = consumeRequiredToken("lookup expression", RSQUARE)
    LookupExpr(previousExpr, lsquare, indexExpr, rsquare)
  }

  private def primaryExpr(): Expr =
    if (NUMBER_LITERAL || STRING_LITERAL || TRUE || FALSE || NULL)
      Literal(nextToken())
    else if (THIS)
      ThisExpr(nextToken())
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
      if ((lookahead(1) == IDENTIFIER || lookahead(1) == STRING_LITERAL) && lookahead(2) == COLON)
        objectExpr() // <-- better error messages if we can positively commit to this being an objectExpr
      else
        speculate("objectExpr")(objectExpr()) getOrElse blockExpr()
    } else if (MISH_INTERPOLATION_START || MISH_INTERPOLATION_START_NO_CAPTURE)
      mishInterpolation()
    else if (forgiving)
      Literal(syntheticToken(STRING_LITERAL))
    else
      unexpectedToken()

  protected def statementSeq(): Expr =
    if (!isLambdaStart)
      actualStatementSeq()
    else
      speculate("lambdaStart")(lambdaStart()) match {
        case Some(start) ⇒
          completeLambdaExpr(start, mayContainStatementSeq = true)
        case None        ⇒
          actualStatementSeq()
      }

  private def actualStatementSeq(): Expr = {
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
    val statements = semisAllowed {
      statementSeq()
    }
    val rbrace = consumeRequiredToken("block", RBRACE)
    BlockExpr(lbrace, statements, rbrace)
  }

  private def parenExpr(): Expr = {
    val lparen = nextToken()
    val expr = statementSeq()
    val rparen = consumeRequiredToken("parenthesis expression", RPAREN)
    ParenExpr(lparen, expr, rparen)
  }

  private def listExpr(): Expr = {
    val lsquare = nextToken()
    if (RSQUARE) {
      val rsquare = nextToken()
      ListExpr(lsquare, None, rsquare)
    } else {
      val firstElement = statementExpr()
      val otherElements =
        safeWhile(COMMA) {
          val comma = nextToken()
          val element = statementExpr()
          comma -> element
        }
      val rsquare = consumeRequiredToken("list", RSQUARE)
      ListExpr(lsquare, Some(ListExprContents(firstElement, otherElements)), rsquare)
    }
  }

}