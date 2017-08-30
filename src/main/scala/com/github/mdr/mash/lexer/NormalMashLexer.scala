package com.github.mdr.mash.lexer

import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.MashParserException

import scala.annotation._

object NormalMashLexer {

  private val LookupStartTokens: Set[TokenType] =
    Set(IDENTIFIER, HOLE, RPAREN, RSQUARE, RBRACE, STRING_LITERAL, NUMBER_LITERAL, NULL, THIS)

  val Keywords: Map[String, TokenType] = Map(
    "and" -> AND,
    "class" -> CLASS,
    "def" -> DEF,
    "else" -> ELSE,
    "false" -> FALSE,
    "if" -> IF,
    "import" -> IMPORT,
    "namespace" -> NAMESPACE,
    "null" -> NULL,
    "or" -> OR,
    "then" -> THEN,
    "this" -> THIS,
    "true" -> TRUE)

  val HoleRegex = "^_[1-9]?$"

}

trait NormalMashLexer { self: MashLexer ⇒

  import NormalMashLexer._

  @tailrec
  protected final def getStringLiteralRest(delimiter: Char): Token = ch match {
    case '`' ⇒
      nextChar()
      ch match {
        case '`' | '$' | 'n' | 'r' | 't' | '"' | '\'' | '~' ⇒
          nextChar()
          getStringLiteralRest(delimiter)
        case c ⇒
          if (forgiving) {
            nextChar()
            getStringLiteralRest(delimiter)
          } else
            throw MashParserException("Invalid string escape `" + c, currentPointedRegion)
      }
    case `delimiter` ⇒
      nextChar()
      token(STRING_LITERAL)
    case _ if afterEof ⇒
      if (forgiving)
        token(STRING_LITERAL)
      else
        throw MashParserException("Unterminated string literal", currentPointedRegion)
    case '$' if delimiter == '"' ⇒
      // We don't consume the dollar, but this signals that the string is interpolated, so we switch into that mode
      modeStack = modeStack :+ StringInterpolationMode
      token(STRING_START)
    case _ ⇒
      nextChar()
      getStringLiteralRest(delimiter)
  }

  private def isDigit(ch: Char): Boolean = (ch: @switch) match {
    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒ true
    case _ ⇒ false
  }

  @tailrec
  private def getFraction(): Token = (ch: @switch) match {
    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒
      nextChar()
      getFraction()
    case _ ⇒
      token(NUMBER_LITERAL)
  }

  @tailrec
  private def getNumberLiteralRest(): Token = (ch: @switch) match {
    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒
      nextChar()
      getNumberLiteralRest()
    case '.' if isDigit(ch(1)) ⇒
      nextChar()
      getFraction()
    case _ ⇒
      token(NUMBER_LITERAL)
  }

  private def getNumberLiteral(): Token = {
    if (ch == '0') {
      nextChar()
      if (ch == '.' && isDigit(ch(1))) {
        nextChar()
        getFraction()
      } else if (isDigit(ch)) {
        if (forgiving)
          getNumberLiteralRest()
        else
          throw MashParserException("Invalid number literal", currentPointedRegion)
      } else
        token(NUMBER_LITERAL)
    } else {
      nextChar()
      getNumberLiteralRest()
    }
  }

  protected def isIdentifierStart(c: Char) = (ch: @switch) match {
    case 'A' | 'B' | 'C' | 'D' | 'E' |
      'F' | 'G' | 'H' | 'I' | 'J' |
      'K' | 'L' | 'M' | 'N' | 'O' |
      'P' | 'Q' | 'R' | 'S' | 'T' |
      'U' | 'V' | 'W' | 'X' | 'Y' |
      'Z' | '_' |
      'a' | 'b' | 'c' | 'd' | 'e' |
      'f' | 'g' | 'h' | 'i' | 'j' |
      'k' | 'l' | 'm' | 'n' | 'o' |
      'p' | 'q' | 'r' | 's' | 't' |
      'u' | 'v' | 'w' | 'x' | 'y' |
      'z' ⇒ true
    case _ ⇒ false
  }

  protected def readNormalToken(): Token = (ch: @switch) match {
    case ' ' | '\t' | '\n' | '\r' ⇒
      nextChar()
      getWhitespaceRest()
    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ⇒
      getNumberLiteral()
    case '"' ⇒
      nextChar()
      getStringLiteralRest('"')
    case '\'' ⇒
      nextChar()
      getStringLiteralRest('\'')
    case '|' ⇒
      nextChar()
      token(PIPE)
    case '(' ⇒
      nextChar()
      if (previousTokenOpt.exists(t ⇒ LookupStartTokens.contains(t.tokenType)))
        token(LPAREN_INVOKE)
      else
        token(LPAREN)
    case ')' ⇒
      nextChar()
      token(RPAREN)
    case '+' ⇒
      nextChar()
      if (ch == '=') {
        nextChar()
        token(PLUS_EQUALS)
      } else
        token(PLUS)
    case '~' ⇒
      nextChar()
      token(TILDE)
    case '*' ⇒
      nextChar()
      if (ch == '=') {
        nextChar()
        token(TIMES_EQUALS)
      } else
        token(TIMES)
    case '/' ⇒
      nextChar()
      if (ch == '=') {
        nextChar()
        token(DIVIDE_EQUALS)
      } else
        token(DIVIDE)
    case ';' ⇒
      nextChar()
      token(SEMI)
    case '.' ⇒
      nextChar()
      if (ch == '.' && ch(1) == '.') {
        nextChar()
        nextChar()
        token(ELLIPSIS)
      } else
        token(DOT)
    case '?' ⇒
      nextChar()
      if (ch == '.') {
        nextChar()
        token(DOT_NULL_SAFE)
      } else
        token(QUESTION)
    case '!' ⇒
      nextChar()
      if (ch == '=') {
        nextChar()
        token(NOT_EQUALS)
      } else if (ch == '!') {
        nextChar()
        if (ch == '{') {
          nextChar()
          modeStack = modeStack :+ MishMode
          token(MISH_INTERPOLATION_START_NO_CAPTURE)
        } else if (forgiving) {
          nextChar()
          token(ERROR)
        } else
          throw MashParserException(s"Unexpected character $ch after '!!'", currentPointedRegion)
      } else if (ch == '{') {
        nextChar()
        modeStack = modeStack :+ MishMode
        token(MISH_INTERPOLATION_START)
      } else if (isMishWordChar(ch)) {
        nextChar()
        getMishWordRest()
      } else if (forgiving) {
        nextChar()
        token(ERROR)
      } else
        throw MashParserException(s"Unexpected character $ch after '!'", currentPointedRegion)
    case '-' ⇒
      nextChar()
      if (ch == '-') {
        nextChar()
        if (ch.isUnicodeIdentifierStart)
          getIdentRest(Some(LONG_FLAG))
        else if (forgiving)
          token(LONG_FLAG)
        else
          throw MashParserException(s"Incomplete flag", currentPointedRegion)
      } else if (ch == '=') {
        nextChar()
        token(MINUS_EQUALS)
      } else if (isDigit(ch))
        getNumberLiteral()
      else if (ch.isUnicodeIdentifierStart)
        getIdentRest(Some(SHORT_FLAG))
      else
        token(MINUS)
    case '>' ⇒
      nextChar()
      (ch: @switch) match {
        case '=' ⇒
          nextChar()
          token(GREATER_THAN_EQUALS)
        case _ ⇒
          token(GREATER_THAN)
      }
    case '<' ⇒
      nextChar()
      (ch: @switch) match {
        case '=' ⇒
          nextChar()
          token(LESS_THAN_EQUALS)
        case _ ⇒
          token(LESS_THAN)
      }
    case '=' ⇒
      nextChar()
      (ch: @switch) match {
        case '=' ⇒
          nextChar()
          token(LONG_EQUALS)
        case '>' ⇒
          nextChar()
          token(RIGHT_ARROW)
        case _ ⇒
          token(SHORT_EQUALS)
      }
    case '#' ⇒
      getComment()
    case 'A' | 'B' | 'C' | 'D' | 'E' |
      'F' | 'G' | 'H' | 'I' | 'J' |
      'K' | 'L' | 'M' | 'N' | 'O' |
      'P' | 'Q' | 'R' | 'S' | 'T' |
      'U' | 'V' | 'W' | 'X' | 'Y' |
      'Z' | '_' |
      'a' | 'b' | 'c' | 'd' | 'e' |
      'f' | 'g' | 'h' | 'i' | 'j' |
      'k' | 'l' | 'm' | 'n' | 'o' |
      'p' | 'q' | 'r' | 's' | 't' |
      'u' | 'v' | 'w' | 'x' | 'y' |
      'z' ⇒
      nextChar()
      getIdentRest()
    case '[' ⇒
      nextChar()
      if (previousTokenOpt.exists(t ⇒ LookupStartTokens.contains(t.tokenType)))
        token(LSQUARE_LOOKUP)
      else
        token(LSQUARE)
    case '{' ⇒
      nextChar()
      modeStack.last.asInstanceOf[NormalMode].braceLevel += 1
      token(LBRACE)
    case '}' ⇒
      nextChar()
      val mode = modeStack.last.asInstanceOf[NormalMode]
      if (mode.braceLevel == 0) {
        if (modeStack.size > 1)
          modeStack = modeStack.init
      } else
        mode.braceLevel -= 1
      token(RBRACE)
    case ']' ⇒
      nextChar()
      token(RSQUARE)
    case ',' ⇒
      nextChar()
      token(COMMA)
    case ':' ⇒
      nextChar()
      token(COLON)
    case '@' ⇒
      nextChar()
      token(AT)
    case _ if afterEof ⇒
      token(EOF)
    case _ ⇒
      if (forgiving) {
        nextChar()
        token(ERROR)
      } else
        throw MashParserException(s"Unexpected character: $ch (${ch.toHexString})", currentPointedRegion)
  }

  protected def getIdentRest(tokenTypeOpt: Option[TokenType] = None): Token = (ch: @switch) match {
    case 'A' | 'B' | 'C' | 'D' | 'E' |
      'F' | 'G' | 'H' | 'I' | 'J' |
      'K' | 'L' | 'M' | 'N' | 'O' |
      'P' | 'Q' | 'R' | 'S' | 'T' |
      'U' | 'V' | 'W' | 'X' | 'Y' |
      'Z' | '_' |
      'a' | 'b' | 'c' | 'd' | 'e' |
      'f' | 'g' | 'h' | 'i' | 'j' |
      'k' | 'l' | 'm' | 'n' | 'o' |
      'p' | 'q' | 'r' | 's' | 't' |
      'u' | 'v' | 'w' | 'x' | 'y' |
      'z' |
      '0' | '1' | '2' | '3' | '4' |
      '5' | '6' | '7' | '8' | '9' ⇒
      nextChar()
      getIdentRest(tokenTypeOpt)
    case _ ⇒
      lazy val holeOpt = if (currentTokenText matches HoleRegex) Some(HOLE) else None
      val tokenType = tokenTypeOpt orElse holeOpt getOrElse Keywords.getOrElse(currentTokenText, IDENTIFIER)
      token(tokenType)
  }

  protected def getComment(): Token = {
    nextChar()

    @tailrec
    def consumeUntilNewline(): Token =
      (ch: @switch) match {
        case '\n' ⇒
          nextChar()
          token(COMMENT)
        case '\r' if ch(1) != '\n' ⇒
          nextChar()
          token(COMMENT)
        case _ if afterEof ⇒
          token(COMMENT)
        case _ ⇒
          nextChar()
          consumeUntilNewline()
      }

    consumeUntilNewline()
  }

  @tailrec
  protected final def getWhitespaceRest(): Token = (ch: @switch) match {
    case ' ' | '\t' | '\n' | '\r' ⇒
      nextChar()
      getWhitespaceRest()
    case _ ⇒
      token(WHITESPACE)
  }

}