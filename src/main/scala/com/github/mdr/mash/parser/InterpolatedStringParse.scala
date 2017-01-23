package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

import scala.collection.mutable.ArrayBuffer

trait InterpolatedStringParse { self: MashParse ⇒

  protected def interpolatedString(): InterpolatedString = {
    val stringStart = nextToken()
    val parts = ArrayBuffer[InterpolationPart]()
    safeWhile(STRING_MIDDLE || STRING_INTERPOLATION_START_COMPLEX || STRING_INTERPOLATION_START_SIMPLE) {
      parts += interpolationPart()
    }
    val end = consumeRequiredToken(STRING_END)
    InterpolatedString(stringStart, parts, end)
  }

  protected def interpolationPart(): InterpolationPart =
    if (STRING_MIDDLE)
      StringPart(nextToken())
    else if (STRING_INTERPOLATION_START_COMPLEX)
      complexInterpolation()
    else
      simpleInterpolation()

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
      val ident = consumeRequiredToken(IDENTIFIER)
      expr = MemberExpr(expr, dot, ident)
    }
    SimpleInterpolation(start, expr)
  }

  private def complexInterpolation(): ComplexInterpolation = {
    val interpolationStart = nextToken()
    val interpolatedExpr = pipeExpr()
    val rbrace = consumeRequiredToken(RBRACE)
    ComplexInterpolation(interpolationStart, interpolatedExpr, rbrace)
  }

}