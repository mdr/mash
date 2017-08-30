package com.github.mdr.mash.lexer

import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.MashParserException

import scala.annotation._

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
        token(STRING_INTERPOLATION_START_SIMPLE)
      else
        throw MashParserException("Invalid string interpolation sequence", currentPointedRegion)
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
      case '`' ⇒
        nextChar()
        ch match {
          case '`' | '$' | 'n' | 'r' | 't' | '"' | '\'' | '~' ⇒
            nextChar()
            getInterpolatedStringPart()
          case c ⇒
            if (forgiving) {
              nextChar()
              getInterpolatedStringPart()
            } else
              throw MashParserException("Invalid string escape `" + c, currentPointedRegion)
        }
      case '"' ⇒
        nextChar()
        endString()
      case _ if afterEof ⇒
        if (forgiving) {
          endString()
        } else
          throw MashParserException("Unterminated string literal", currentPointedRegion)
      case '$' ⇒
        token(STRING_MIDDLE)
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
        throw MashParserException("Invalid string interpolation sequence", currentPointedRegion) // This actually shouldn't happen
    if (!isIdentifierStart(ch) && ch != '.')
      modeStack = modeStack.init
    tok
  }

}