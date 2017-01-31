package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

import scala.collection.mutable.ArrayBuffer

trait MishParse { self: MashParse ⇒

  def mishExpr(): MishExpr = {
    val command = mishItem()
    val args = safeWhile(MISH_WORD || STRING_LITERAL || STRING_START || STRING_INTERPOLATION_START_SIMPLE || STRING_INTERPOLATION_START_COMPLEX || LESS_THAN || GREATER_THAN) {
      mishItem()
    }
    MishExpr(command, args)
  }

  private def mishItem(redirectsAllowed: Boolean = true): MishItem = {
    if (MISH_WORD)
      MishWord(nextToken())
    else if (STRING_LITERAL)
      MishString(Literal(nextToken()))
    else if (STRING_START)
      MishString(interpolatedString())
    else if (STRING_INTERPOLATION_START_SIMPLE || STRING_INTERPOLATION_START_COMPLEX)
      MishInterpolation(interpolationPart())
    else if (LESS_THAN || GREATER_THAN) {
      val op = nextToken()
      val item = mishItem(redirectsAllowed = false)
      MishRedirect(op, item)
    } else if (forgiving)
      MishWord(syntheticToken(MISH_WORD))
    else
      unexpectedToken()
  }

  protected def mishInterpolation(): MishInterpolationExpr = {
    val start = nextToken()
    val expr = mishExpr()
    val rbrace = consumeRequiredToken("interpolation", RBRACE)
    MishInterpolationExpr(start, expr, rbrace)
  }

}