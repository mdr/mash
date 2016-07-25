package com.github.mdr.mash.lexer

import scala.annotation._
import com.github.mdr.mash.lexer.TokenType._
import scala.PartialFunction.cond
import com.github.mdr.mash.parser.MashParserException

object MishLexer {

  private val ReservedPunctuation = """$"'#}<>"""

}

trait MishLexer { self: MashLexer ⇒

  import MishLexer._

  protected def readMishToken(): Token = ch match {
    case c if isMishWordChar(c) ⇒
      nextChar()
      getMishWordRest()
    case ' ' | '\t' | '\n' | '\r' ⇒
      nextChar()
      getWhitespaceRest()
    case '$' ⇒
      readStringInterpolationToken()
    case '#' ⇒
      getComment()
    case '>' =>
      nextChar()
      token(GREATER_THAN)
    case '<' =>
      nextChar()
      token(LESS_THAN)
    case '}' ⇒
      nextChar()
      if (modeStack.size > 1)
        modeStack = modeStack.init
      token(RBRACE)
    case '"' ⇒
      nextChar()
      getStringLiteralRest('"')
    case '\'' ⇒
      nextChar()
      getStringLiteralRest('\'')
    case _ if afterEof ⇒
      token(EOF)
    case _ ⇒
      if (forgiving) {
        nextChar()
        token(ERROR)
      } else
        throw new MashParserException(s"Unexpected character: $ch (${ch.toHexString})", currentPointedRegion)
  }

  protected def getMishWordRest(): Token = {
    while (isMishWordChar(ch))
      nextChar()
    token(MISH_WORD)
  }

  protected def isMishWordChar(c: Char): Boolean =
    !afterEof && !c.isWhitespace && !ReservedPunctuation.contains(c)

}