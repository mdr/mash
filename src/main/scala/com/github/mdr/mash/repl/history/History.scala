package com.github.mdr.mash.repl.history

import java.nio.file.Path
import java.util.regex.Pattern

import com.github.mdr.mash.lexer.{ MashLexer, Token, TokenType }
import com.github.mdr.mash.repl.handler.HistoricalArgumentSource
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.utils.Region

object History {

  case class Match(command: String, region: Region)

}

trait History extends HistoricalArgumentSource {

  import History._

  def record(cmd: String, commandNumber: Int, mish: Boolean, resultOpt: Option[MashValue], workingDirectory: Path)

  def resetHistoryPosition(): Unit

  def forgetInProgressCommand(): Unit

  def goForwards(): Option[String]

  def goBackwards(inProgressCommand: String): Option[String]

  def getHistory: Seq[HistoryEntry]

  def findMatches(searchString: String): Seq[Match] = {
    val pattern = Pattern.compile(Pattern.quote(searchString), Pattern.CASE_INSENSITIVE)
    def tryMatch(command: String) = {
      val matcher = pattern.matcher(command)
      val found = matcher.find()
      if (found) {
        val start = matcher.start
        val end = matcher.end
        val length = end - start
        Some(Match(command, Region(start, length)))
      } else
        None
    }
    getHistory.map(_.command).flatMap(tryMatch)
  }

  def getHistoricalArguments(lastArgIndex: Int): Option[String] =
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