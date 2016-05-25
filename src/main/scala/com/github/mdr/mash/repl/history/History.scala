package com.github.mdr.mash.repl.history

import java.io.File
import java.nio.file.Path
import java.time.Instant
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.lexer.TokenType

case class HistoryEntry(timestamp: Instant, command: String, mish: Boolean)

object History {

  lazy val MashDir: Path = new File(System.getProperty("user.home"), ".mash").toPath

}

trait History {

  def record(cmd: String, mish: Boolean)

  def resetHistoryPosition()

  def goForwards(): Option[String]

  def goBackwards(): Option[String]

  def getHistory: Seq[HistoryEntry]

  def findMatches(searchString: String): Seq[String] =
    getHistory.map(_.command).filter(_.contains(searchString))

  def getLastArg(lastArgIndex: Int): Option[String] =
    getHistory.map(_.command).flatMap(getLastArg(_)).drop(lastArgIndex).headOption

  private def getLastArg(s: String): Option[String] = {
    val tokens = MashLexer.tokenise(s, includeCommentsAndWhitespace = false, forgiving = true, mish = false)
    tokens.filter { t ⇒
      val tokenType = t.tokenType
      tokenType.isIdentifier || tokenType.isKeyword || tokenType.isLiteral || tokenType == TokenType.MISH_WORD
    }.lastOption.map(_.text)
  }

}
