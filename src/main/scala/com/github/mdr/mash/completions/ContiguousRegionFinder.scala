package com.github.mdr.mash.completions

import com.github.mdr.mash.lexer._
import com.github.mdr.mash.utils.Region

object ContiguousRegionFinder {

  private val IlliberalStopTokens: Set[TokenType] = {
    import TokenType._
    Set[TokenType](LPAREN, RPAREN, LBRACE, RBRACE, LSQUARE, RSQUARE, MISH_INTERPOLATION_START, MISH_INTERPOLATION_START)
  }

  /**
   * Expand a region by adding tokens either side of it, as long as they aren't whitespace.
   */
  def getContiguousRegion(s: String, initialRegion: Region, mish: Boolean, liberal: Boolean): Region = {
    val tokens = MashLexer.tokenise(s, forgiving = true, includeCommentsAndWhitespace = true, mish = mish)

    val leftTokenIndex = tokens.indexWhere(_.region contains initialRegion.offset)
    if (leftTokenIndex < 0)
      return initialRegion

    val rightTokenIndex = tokens.indexWhere(_.region contains initialRegion.lastPos)
    if (rightTokenIndex < 0)
      return initialRegion

    var start = leftTokenIndex
    while (start > 0 && keep(tokens(start - 1), liberal))
      start -= 1

    var end = rightTokenIndex
    while (end + 1 < tokens.length && keep(tokens(end + 1), liberal))
      end += 1

    val startToken = tokens(start)
    val startPos = startToken.region.offset
    val endToken = tokens(end)
    Region(startPos, endToken.region.posAfter - startPos)
  }

  private def keep(token: Token, liberal: Boolean) = {
    import TokenType._
    !token.isWhitespace && !token.isEof && (liberal || !IlliberalStopTokens.contains(token.tokenType))
  }

}