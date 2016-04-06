package com.github.mdr.mash.completions

import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.lexer.Token

object ContiguousRegionFinder {

  /**
   * Expand a region by adding tokens either side of it, as long as they aren't whitespace.
   */
  def getContiguousRegion(s: String, initialRegion: Region, mish: Boolean): Region = {
    val tokens = MashLexer.tokenise(s, forgiving = true, includeCommentsAndWhitespace = true, mish = mish)

    val leftTokenIndex = tokens.indexWhere(_.region contains initialRegion.offset)
    if (leftTokenIndex < 0)
      return initialRegion

    val rightTokenIndex = tokens.indexWhere(_.region contains initialRegion.lastPos)
    if (rightTokenIndex < 0)
      return initialRegion

    var start = leftTokenIndex
    while (start > 0 && keep(tokens(start - 1)))
      start -= 1

    var end = rightTokenIndex
    while (end + 1 < tokens.length && keep(tokens(end + 1)))
      end += 1

    val startToken = tokens(start)
    val startPos = startToken.region.offset
    val endToken = tokens(end)
    Region(startPos, endToken.region.posAfter - startPos)
  }

  private def keep(token: Token) = !token.isWhitespace && !token.isEof

}