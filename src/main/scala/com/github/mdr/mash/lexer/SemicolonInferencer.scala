package com.github.mdr.mash.lexer

object SemicolonInferencer {
  import TokenType._

  private val CanStartAStatement: Set[TokenType] =
    Set(IDENTIFIER, RIGHT_ARROW, MINUS, LPAREN, LBRACE, LSQUARE, HOLE, TRUE, FALSE, STRING_LITERAL,
      NUMBER_LITERAL, NULL, IF, STRING_START, STRING_INTERPOLATION_START_COMPLEX, STRING_INTERPOLATION_START_SIMPLE,
      MISH_INTERPOLATION_START, MISH_INTERPOLATION_START_NO_CAPTURE)

  private val CanEndAStatement: Set[TokenType] =
    Set(IDENTIFIER, RPAREN, RBRACE, RSQUARE, HOLE, TRUE, FALSE, STRING_LITERAL, NUMBER_LITERAL, NULL, STRING_END, EOF,
      MISH_WORD, QUESTION)

  def getInferredSemicolonCandidates(pruneResult: PruneResult): Set[Token] = {
    val PruneResult(tokens, intertokenMap) = pruneResult
    val inferredSemicolonCandidates =
      for {
        (previousToken, token) ← tokens zip tokens.drop(1)
        if intertokenMap(token) exists (_.text contains "\n")
        if CanEndAStatement.contains(previousToken.tokenType)
        if CanStartAStatement.contains(token.tokenType)
      } yield token
    inferredSemicolonCandidates.toSet
  }

  private def canStartAStatement(tokenType: TokenType): Boolean =
    CanStartAStatement.contains(tokenType)

  private def canEndAStatement(tokenType: TokenType): Boolean =
    CanEndAStatement.contains(tokenType)

}