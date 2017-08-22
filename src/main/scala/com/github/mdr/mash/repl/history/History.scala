package com.github.mdr.mash.repl.history

import java.nio.file.Path
import java.time.Instant
import java.util.regex.Pattern

import com.github.mdr.mash.lexer.{ MashLexer, Token, TokenType }
import com.github.mdr.mash.repl.handler.HistoricalArgumentSource
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.Utils._

object History {

  case class Match(command: String, region: Region, timestamp: Instant, workingDirectory: Path)

}

trait History extends HistoricalArgumentSource {

  import History._

  def record(cmd: String,
             commandNumber: Int,
             workingDirectory: Path,
             mish: Boolean = false,
             resultOpt: Option[MashValue] = None)

  def resetHistoryPosition(): Unit

  def forgetInProgressCommand(): Unit

  def goForwards(): Option[String]

  def goBackwards(inProgressCommand: String): Option[String]

  def getHistory: Seq[HistoryEntry]

  /**
    * Search backwards through history to find a match for a given search string.
    */
  def findMatch(searchString: String, index: Int, directoryOpt: Option[Path]): Option[Match] =
    findMatches(searchString, directoryOpt).drop(index).headOption

  def findLastMatch(searchString: String, directoryOpt: Option[Path]): Option[(Match, Int)] =
    findMatches(searchString, directoryOpt).zipWithIndex.lastOption

  private def findMatches(searchString: String, directoryOpt: Option[Path]): Stream[Match] = {
    val pattern = Pattern.compile(Pattern.quote(searchString), Pattern.CASE_INSENSITIVE)

    def tryMatch(entry: HistoryEntry): Option[Match] = {
      val matcher = pattern.matcher(entry.command)
      matcher.find().option {
        val region = Region.fromStartEnd(matcher.start, matcher.end)
        Match(entry.command, region, entry.timestamp, entry.workingDirectory)
      }
    }

    getHistory.toStream
      .flatMap(tryMatch)
      .filter(m ⇒ directoryOpt.forall(m.workingDirectory == _))
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