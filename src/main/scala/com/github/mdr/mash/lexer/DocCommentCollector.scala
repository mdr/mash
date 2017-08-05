package com.github.mdr.mash.lexer

import com.github.mdr.mash.utils.Region

import scala.collection.mutable.ArrayBuffer
import com.github.mdr.mash.utils.Utils._

case class DocComment(commentTokens: Seq[Token], text: String) {

  def region: Region = commentTokens.map(_.region).reduce(_ merge _)

}

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
    commentTokens.nonEmpty.option {
      val commentContent = commentTokens.reverse.map(_.text.tail).mkString
      DocComment(commentTokens, commentContent)
    }
  }

}
