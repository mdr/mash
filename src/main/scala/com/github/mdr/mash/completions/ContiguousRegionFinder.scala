package com.github.mdr.mash.completions

import com.github.mdr.mash.lexer._
import com.github.mdr.mash.utils.{ Region, Utils }

import scala.annotation.tailrec

object ContiguousRegionFinder {

  private val IlliberalStopTokens: Set[TokenType] = {
    import TokenType._
    Set[TokenType](LPAREN, LPAREN_INVOKE, RPAREN, LBRACE, RBRACE, LSQUARE, LSQUARE_LOOKUP, RSQUARE,
      MISH_INTERPOLATION_START, MISH_INTERPOLATION_START, SHORT_EQUALS)
  }

  /**
   * Expand a region by adding tokens either side of it, as long as they aren't whitespace.
   */
  def getContiguousRegion(s: String, initialRegion: Region, mish: Boolean, liberal: Boolean): Region = {
    val tokens = MashLexer.tokenise(s, forgiving = true, mish = mish).rawTokens

    val startToken = scanTokensLeft(tokens, initialRegion, liberal).getOrElse(return initialRegion)
    val endToken = scanTokensRight(tokens, initialRegion, liberal).getOrElse(return initialRegion)

    val startPos = startToken.region.offset
    Region(startPos until endToken.region.posAfter)
  }

  private def scanTokensLeft(tokens: Seq[Token], initialRegion: Region, liberal: Boolean): Option[Token] = {
    val startPosOpt = Utils.indexWhere[Token](tokens, _.region contains initialRegion.offset)
    startPosOpt.map { leftTokenIndex ⇒
      @tailrec def scanLeft(pos: Int): Int =
        if (pos > 0 && keep(tokens(pos - 1), liberal))
          scanLeft(pos - 1)
        else
          pos
      tokens(scanLeft(leftTokenIndex))
    }
  }

  private def scanTokensRight(tokens: Seq[Token], initialRegion: Region, liberal: Boolean): Option[Token] = {
    val startPosOpt = Utils.indexWhere[Token](tokens, _.region contains initialRegion.lastPos)
    startPosOpt.map { rightTokenIndex ⇒
      @tailrec def scanRight(pos: Int): Int =
        if (pos + 1 < tokens.length && keep(tokens(pos + 1), liberal))
          scanRight(pos + 1)
        else
          pos
      tokens(scanRight(rightTokenIndex))
    }
  }

  private def keep(token: Token, liberal: Boolean) = {
    !token.isWhitespace && !token.isEof && (liberal || !IlliberalStopTokens.contains(token.tokenType))
  }

}