package com.github.mdr.mash.lexer

object SemicolonInferencer {

  import TokenType._

  /**
    * All tokens types that can start a statement in Mash syntax.
    *
    * Note: DOT and DOT_NULL_SAFE can start a statement, but we consider them not to for this purpose, so that:
    * listFiles
    *   .permissions
    *   .owner
    *   .canRead
    * is interpreted in the obvious way.
    */
  private val CanStartAStatement: Set[TokenType] =
    Set(IDENTIFIER, RIGHT_ARROW, MINUS, LPAREN, LBRACE, LSQUARE, HOLE, TRUE, FALSE, STRING_LITERAL,
      NUMBER_LITERAL, NULL, IF, STRING_START, STRING_INTERPOLATION_START_COMPLEX, STRING_INTERPOLATION_START_SIMPLE,
      MISH_INTERPOLATION_START, MISH_INTERPOLATION_START_NO_CAPTURE, DEF, CLASS, THIS, AT, IMPORT)

  private val CanEndAStatement: Set[TokenType] =
    Set(IDENTIFIER, RPAREN, RBRACE, RSQUARE, HOLE, TRUE, FALSE, STRING_LITERAL, NUMBER_LITERAL, NULL, STRING_END, EOF,
      MISH_WORD, QUESTION, THIS, LONG_FLAG, SHORT_FLAG)

  def getInferredSemicolonCandidates(pruneResult: PruneResult): Set[Token] = {
    val PruneResult(tokens, intertokenMap) = pruneResult
    val inferredSemicolonCandidates =
      for {
        (previousToken, token) ← tokens zip tokens.drop(1)
        if intertokenMap(token) exists (_.text contains "\n")
        if CanEndAStatement contains previousToken.tokenType
        if CanStartAStatement contains token.tokenType
      } yield token
    inferredSemicolonCandidates.toSet
  }

}