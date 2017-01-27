package com.github.mdr.mash.repl.history

import java.nio.file.Path

import com.github.mdr.mash.lexer.{ MashLexer, Token, TokenType }
import com.github.mdr.mash.runtime.MashValue

trait History {

  def record(cmd: String, commandNumber: Int, mish: Boolean, resultOpt: Option[MashValue], workingDirectory: Path)

  def resetHistoryPosition(): Unit

  def forgetInProgressCommand(): Unit

  def goForwards(): Option[String]

  def goBackwards(inProgressCommand: String): Option[String]

  def getHistory: Seq[HistoryEntry]

  def findMatches(searchString: String): Seq[String] =
    getHistory.map(_.command).filter(_.contains(searchString))

  def getLastArg(lastArgIndex: Int): Option[String] =
    getHistory.toStream.flatMap(getLastArg(_)).drop(lastArgIndex).headOption

  private def getLastArg(historyEntry: HistoryEntry): Option[String] =
    MashLexer.tokenise(historyEntry.command, forgiving = true, mish = historyEntry.mish)
      .tokens
      .filter(isLastArgCandidate)
      .lastOption
      .map(_.text)

  private def isLastArgCandidate(token: Token): Boolean = {
    val tokenType = token.tokenType
    tokenType.isIdentifier || tokenType.isKeyword || tokenType.isLiteral || tokenType == TokenType.MISH_WORD
  }

  /**
    * Commit to editing this line, as opposed to scrolling past it in history
    */
  def commitToEntry(): Unit

  def isCommittedToEntry: Boolean

}