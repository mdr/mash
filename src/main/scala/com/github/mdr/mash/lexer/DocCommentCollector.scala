package com.github.mdr.mash.lexer

import scala.collection.mutable.ArrayBuffer

case class DocComment(text: String)

object DocCommentCollector {

  def findDocComments(pruneResult: PruneResult): Map[Token, DocComment] = {
    val pairs =
      for {
        token ← pruneResult.tokens
        preceedingIntertokens ← pruneResult.intertokenMap.get(token)
        docComment ← findDocComment(token, preceedingIntertokens)
      } yield token -> docComment
    pairs.toMap
  }

  private def findDocComment(token: Token, preceedingTokens: Seq[Token]): Option[DocComment] = {
    val tokenStream = preceedingTokens.reverse
    var pos = 0
    def currentType: TokenType = if (pos >= tokenStream.length) TokenType.EOF else tokenStream(pos).tokenType
    val commentTokens: ArrayBuffer[Token] = ArrayBuffer()
    var continue = true
    while (continue) {
      if (currentType == TokenType.WHITESPACE)
        pos += 1
      if (currentType == TokenType.COMMENT) {
        commentTokens += tokenStream(pos)
        pos += 1
      } else
        continue = false
    }
    if (commentTokens.isEmpty)
      None
    else
      Some(DocComment(commentTokens.reverse.map(_.text.tail).mkString))
  }

}
