package com.github.mdr.mash.lexer

import scala.annotation._
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.MashParserException

trait StringInterpolationLexer { self: MashLexer ⇒

  protected def readStringInterpolationToken(): Token = (ch: @switch) match {
    case '$' ⇒
      nextChar()
      if (ch == '{') {
        nextChar()
        modeStack = modeStack :+ NormalMode()
        token(STRING_INTERPOLATION_START_COMPLEX)
      } else if (isIdentifierStart(ch)) {
        modeStack = modeStack :+ StringInterpolationIdentifierMode
        token(STRING_INTERPOLATION_START_SIMPLE)
      } else if (forgiving)
        readStringInterpolationToken()
      else
        throw new MashParserException("Invalid string interpolation sequence", currentPointedRegion)
    case _ ⇒
      getInterpolatedStringPart()
  }

  @tailrec
  private def getInterpolatedStringPart(): Token = {
    def endString(): Token = {
      // Only pop the stack if we are interpolating a string, not a mish fragment:
      if (modeStack.last == StringInterpolationMode)
        modeStack = modeStack.init
      token(STRING_END)
    }
    ch match {
      case '$' ⇒
        token(STRING_MIDDLE)
      case '"' ⇒
        nextChar()
        endString()
      case '\n' | '\r' ⇒
        if (forgiving) {
          nextChar()
          endString()
        } else
          throw new MashParserException("Unterminated string literal", currentPointedRegion)
      case _ if afterEof ⇒
        if (forgiving) {
          endString()
        } else
          throw new MashParserException("Unterminated string literal", currentPointedRegion)
      case _ ⇒
        nextChar()
        getInterpolatedStringPart()
    }
  }

  protected def readStringInterpolationIdentifierToken(): Token = {
    val tok =
      if (isIdentifierStart(ch)) {
        nextChar()
        getIdentRest()
      } else if (ch == '.') {
        nextChar()
        token(DOT)
      } else
        throw new MashParserException("Invalid string interpolation sequence", currentPointedRegion) // This actually shouldn't happen
    if (!isIdentifierStart(ch) && ch != '.')
      modeStack = modeStack.init
    tok
  }

}