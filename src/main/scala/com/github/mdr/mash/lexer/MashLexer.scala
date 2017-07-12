package com.github.mdr.mash.lexer

import com.github.mdr.mash.lexer.NormalMashLexer.Keywords
import com.github.mdr.mash.parser.MashParserException
import com.github.mdr.mash.utils.{ PointedRegion, Region }

case class LexerResult(tokens: Seq[Token],
                       rawTokens: Seq[Token],
                       inferredSemicolonCandidates: Set[Token],
                       docComments: Map[Token, DocComment])

object MashLexer {

  def isLegalIdentifier(s: String): Boolean =
    s.matches("^[A-Za-z_][A-Za-z0-9_]*$") && !Keywords.contains(s) && !s.matches(NormalMashLexer.HoleRegex)

  private final val EOF_CHAR = '\u001A' // Dummy character used after EOF (in lookaheads etc)

  /**
    * @param forgiving -- if true, will not throw any exceptions on encountering errors tokenising
    */
  @throws[MashParserException]
  def tokenise(s: String, forgiving: Boolean = true, mish: Boolean = false): LexerResult = {
    val initialMode = if (mish) MishMode else NormalMode()
    val lexer = new MashLexer(s, forgiving, initialMode)
    val rawTokens = lexer.toSeq
    val pruneResult = pruneCommentsAndWhitespace(rawTokens)
    val inferredSemicolonCandidates = SemicolonInferencer.getInferredSemicolonCandidates(pruneResult)
    val docComments = DocCommentCollector.findDocComments(pruneResult)
    LexerResult(pruneResult.tokens, rawTokens, inferredSemicolonCandidates, docComments)
  }

  private def pruneCommentsAndWhitespace(rawTokens: Seq[Token]) = {
    var intertokenMap: Map[Token, Seq[Token]] = Map()
    var currentIntertokens: Seq[Token] = Seq()
    var tokens: Seq[Token] = Seq()
    for (token ← rawTokens) {
      if (token.isComment || token.isWhitespace)
        currentIntertokens = currentIntertokens :+ token
      else {
        intertokenMap += token -> currentIntertokens
        currentIntertokens = Seq()
        tokens = tokens :+ token
      }
    }
    PruneResult(tokens, intertokenMap)
  }

}

/**
  * @param tokens        -- tokens after filtering out whitespace and comment tokens
  * @param intertokenMap -- for each regular token, any whitespace and comment tokens present before it
  */
private[lexer] case class PruneResult(tokens: Seq[Token], intertokenMap: Map[Token, Seq[Token]])

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

  override def hasNext = !complete

  override def next(): Token = {
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