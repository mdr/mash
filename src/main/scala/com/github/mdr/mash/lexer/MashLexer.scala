package com.github.mdr.mash.lexer

import scala.annotation._
import com.github.mdr.mash.utils.PointedRegion
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ParseError
import com.github.mdr.mash.parser.MashParserException

object MashLexer {

  private final val EOF_CHAR = '\u001A' // Dummy character used after EOF (in lookaheads etc)

  /**
   * @param forgiving -- if true, will not throw any exceptions on encountering errors tokenising
   */
  @throws[MashParserException]
  def tokenise(s: String, includeCommentsAndWhitespace: Boolean = false, forgiving: Boolean = true, mish: Boolean = false): Seq[Token] = {
    val initialMode = if (mish) MishMode else NormalMode()
    val lexer = new MashLexer(s, forgiving, initialMode)
    val rawTokens = lexer.toSeq
    if (includeCommentsAndWhitespace)
      rawTokens
    else
      rawTokens.filterNot(t ⇒ t.isWhitespace || t.isComment)
  }

}

class MashLexer(s: String, protected val forgiving: Boolean = true, initialMode: LexerMode = NormalMode())
    extends NormalMashLexer with StringInterpolationLexer with MishLexer with Iterator[Token] {

  import MashLexer._

  /**
   * Stack of lexical contexts (e.g. normal Mash inside a string interpolation inside a Mish expression).
   */
  protected var modeStack: Seq[LexerMode] = Seq(initialMode)

  /**
   * Current position within the string s.
   */
  private var pos: Int = 0

  /**
   * Position of the start of the token currently being recognised.
   */
  private var currentTokenStart: Int = 0

  /**
   * If all the tokens have been consumed
   */
  private var complete = false

  /**
   * The previous token emitted
   */
  protected var previousTokenOpt: Option[Token] = None

  /**
   * Returns true if we've progressed passed the end of the input
   */
  protected def afterEof: Boolean = pos >= s.length

  /**
   * Fetch the current character, or EOF_MARKER if passed the end of the input
   */
  protected def ch: Char = ch(0)

  /**
   * Fetch the character some number of positions ahead of the current character, or EOF_MARKER if passed the end of the
   * input.
   */
  protected def ch(lookahead: Int): Char = {
    val newPos = pos + lookahead
    if (newPos >= s.length)
      EOF_CHAR
    else
      s(newPos)
  }

  /**
   * Consume the current character and advance to the next.
   */
  protected def nextChar() {
    if (pos < s.length)
      pos += 1
  }

  /**
   * Complete the token currently being processed.
   */
  protected def token(tokenType: TokenType): Token = {
    val region = currentTokenRegion
    Token(tokenType, region.offset, region.length, s)
  }

  protected def currentTokenText: String = s.substring(currentTokenStart, pos)

  private def currentTokenRegion: Region =
    Region(currentTokenStart, pos - currentTokenStart)

  protected def currentPointedRegion = PointedRegion(pos, currentTokenRegion)

  def hasNext = !complete

  def next(): Token = {
    if (complete)
      throw new IllegalStateException("All tokens have been read")
    val token = readToken()
    previousTokenOpt = Some(token)
    currentTokenStart = pos
    if (token.isEof)
      complete = true
    token
  }

  private def readToken(): Token = modeStack.last match {
    case NormalMode()                      ⇒ readNormalToken()
    case StringInterpolationMode           ⇒ readStringInterpolationToken()
    case StringInterpolationIdentifierMode ⇒ readStringInterpolationIdentifierToken()
    case MishMode                          ⇒ readMishToken()
  }

}