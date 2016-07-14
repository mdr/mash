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
import com.github.mdr.mash.lexer.TokenType._

class Parse(lexerResult: LexerResult, initialForgiving: Boolean) {

  private val tokens = lexerResult.tokens.toArray

  /**
   * Index of the token currently being examined
   */
  private var pos = 0

  /**
   * Whether we are currently forgiving; we might temporarily stop being forgiving to do speculative parsing.
   */
  protected var forgiving: Boolean = initialForgiving

  /**
   * Indicate that a synthetic inferred semi was just inferred
   */
  private var inferredSemi: Boolean = false

  /**
   * Return the current token. If it's past the end of the token sequence, then return the last token (EOF).
   */
  protected def currentToken: Token = {
    val token = currentSequenceToken
    if (shouldInferSemicolon(token))
      syntheticToken(SEMI, token)
    else
      token
  }

  private def currentSequenceToken =
    if (pos < tokens.length)
      tokens(pos)
    else
      tokens.last

  private def shouldInferSemicolon(token: Token) = lexerResult.inferredSemicolonCandidates.contains(token) && !inferredSemi

  private def currentTokenType = currentToken.tokenType

  private def currentPos = currentToken.offset

  protected def currentLocation = PointedRegion(currentToken.offset, currentToken.region)

  /**
   *  Consume the current token, and advance to the next.
   *  @return the token before advancing
   */
  protected def nextToken(): Token = {
    val token = currentSequenceToken
    if (shouldInferSemicolon(token)) {
      inferredSemi = true
      syntheticToken(SEMI)
    } else {
      inferredSemi = false
      pos += 1
      token
    }
  }

  /**
   * We're testing token types a lot, a bit of shorthand helps.
   */
  protected implicit def tokenType2Boolean(tokenType: TokenType): Boolean = currentTokenType == tokenType

  protected def errorExpectedToken(expected: String) =
    throw new MashParserException(s"Expected '$expected', but instead found '${currentToken.text}'", currentLocation)

  /**
   * Speculatively parse from the current position. If it succeeds, we return Some(..), and any consumed tokens
   * remain consumed. Otherwise, we return None, and the state of the parse remains unchanged.
   */
  protected def speculate[T](p: ⇒ T): Option[T] = {
    val oldPos = pos
    val oldInferredSemi = inferredSemi
    val oldForgiving = forgiving
    forgiving = false
    try
      Some(p)
    catch {
      case _: MashParserException ⇒
        pos = oldPos
        inferredSemi = oldInferredSemi
        None
    } finally
      forgiving = oldForgiving
  }

  protected def syntheticToken(tokenType: TokenType): Token =
    syntheticToken(tokenType, afterTokenOpt = None)

  protected def syntheticToken(tokenType: TokenType, afterToken: Token): Token =
    syntheticToken(tokenType, Some(afterToken))

  protected def syntheticToken(tokenType: TokenType, afterTokenOpt: Option[Token]): Token =
    Token(tokenType, afterTokenOpt.map(_.region.posAfter).getOrElse(0), 0, afterTokenOpt.map(_.source).getOrElse(""))

  /**
   * While loop that errors if it's not making progress
   */
  protected def safeWhile(cond: ⇒ Boolean)(body: ⇒ Any) {
    var oldPos = pos
    var noAdvanceCount = 0
    while (cond) {
      body
      if (oldPos == pos)
        noAdvanceCount += 1
      else
        noAdvanceCount = 0
      assert(noAdvanceCount < 10, "Infinite loop detected parsing at position " + pos + ", current token is " + currentToken)
      oldPos = pos
    }
  }

  protected def unexpectedToken() =
    if (EOF)
      throw new MashParserException(s"Unexpected end-of-input", currentLocation)
    else
      throw new MashParserException(s"Unexpected token '${currentToken.text}'", currentLocation)

}