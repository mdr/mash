package com.github.mdr.mash.completions

import com.github.mdr.mash.lexer._
import com.github.mdr.mash.utils.Region
import scala.annotation.tailrec
import com.github.mdr.mash.utils.Utils

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

    val startToken = scanTokensLeft(tokens, initialRegion, liberal).getOrElse(return initialRegion)
    val endToken = scanTokensRight(tokens, initialRegion, liberal).getOrElse(return initialRegion)

    val startPos = startToken.region.offset
    Region(startPos until endToken.region.posAfter)
  }

  private def scanTokensLeft(tokens: Seq[Token], initialRegion: Region, liberal: Boolean): Option[Token] =
    Utils.indexWhere(tokens, (t: Token) ⇒ t.region contains initialRegion.offset).map { leftTokenIndex ⇒
      @tailrec
      def scanLeft(pos: Int): Int =
        if (pos > 0 && keep(tokens(pos - 1), liberal))
          scanLeft(pos - 1)
        else
          pos
      val start = scanLeft(leftTokenIndex)
      tokens(start)
    }

  private def scanTokensRight(tokens: Seq[Token], initialRegion: Region, liberal: Boolean): Option[Token] =
    Utils.indexWhere(tokens, (t: Token) ⇒ t.region contains initialRegion.lastPos).map { rightTokenIndex ⇒
      @tailrec
      def scanRight(pos: Int): Int =
        if (pos + 1 < tokens.length && keep(tokens(pos + 1), liberal))
          scanRight(pos + 1)
        else
          pos
      val end = scanRight(rightTokenIndex)
      tokens(end)
    }

  private def keep(token: Token, liberal: Boolean) = {
    import TokenType._
    !token.isWhitespace && !token.isEof && (liberal || !IlliberalStopTokens.contains(token.tokenType))
  }

}