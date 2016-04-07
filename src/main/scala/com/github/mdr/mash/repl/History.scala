package com.github.mdr.mash.repl

import org.apache.commons.io.FileUtils
import scala.collection.JavaConverters._
import java.io.File
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.lexer.TokenType
import java.time.Instant
import scala.util.control.Exception._
import java.time.Clock
import com.fatboyindustrial.gsonjavatime.Converters
import com.google.gson.GsonBuilder

case class HistoryEntry(timestamp: Instant, command: String, mish: Boolean)

object History {

  private val NotInHistory = -1

  val mashDir = new File(System.getProperty("user.home"), ".mash")

  private val historyFile = new File(mashDir, "history")

}

class History {

  private val clock: Clock = Clock.systemDefaultZone

  private val gson = Converters.registerAll(new GsonBuilder()).create()

  import History._

  private def loadHistoryEntry(s: String): HistoryEntry = {
    val entry =
      try
        gson.fromJson(s, classOf[HistoryEntry])
      catch {
        case _: Exception ⇒
          HistoryEntry(clock.instant, s, mish = false)
      }
    val newEntry =
      if (entry == null || entry.command == null)
        HistoryEntry(clock.instant, s, mish = false)
      else
        entry
    newEntry
  }

  private def loadHistory(): Seq[HistoryEntry] =
    if (historyFile.exists)
      FileUtils.readLines(historyFile).asScala.reverse.map(loadHistoryEntry)
    else
      Seq()

  var history: Seq[HistoryEntry] = loadHistory()
  var historyPos = NotInHistory

  def resetHistoryPosition() = historyPos = NotInHistory

  def goForwards(): Option[String] =
    if (historyPos >= 0) {
      historyPos -= 1
      if (historyPos != NotInHistory)
        Some(history(historyPos).command)
      else
        Some("")
    } else
      None

  def goBackwards(): Option[String] =
    if (historyPos < history.size - 1) {
      historyPos += 1
      Some(history(historyPos).command)
    } else
      None

  def record(cmd: String, mish: Boolean) {
    val entry = HistoryEntry(clock.instant, cmd, mish)
    history = entry +: history
    if (!mashDir.exists)
      mashDir.mkdir()
    val json = gson.toJson(entry)
    FileUtils.writeLines(historyFile, Seq(json).asJava, true)
  }

  def findMatches(searchString: String): Seq[String] =
    history.map(_.command).filter(_.contains(searchString))

  def getLastArg(lastArgIndex: Int): Option[String] =
    history.map(_.command).flatMap(getLastArg(_)).drop(lastArgIndex).headOption

  private def getLastArg(s: String): Option[String] = {
    val tokens = MashLexer.tokenise(s, includeCommentsAndWhitespace = false, forgiving = true, mish = false)
    tokens.filter { t ⇒
      val tokenType = t.tokenType
      tokenType.isIdentifier || tokenType.isKeyword || tokenType.isLiteral || tokenType == TokenType.MISH_WORD
    }.lastOption.map(_.text)
  }

  def getHistory: Seq[HistoryEntry] = history.sortBy(_.timestamp)

}