package com.github.mdr.mash.repl.history

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.time.Clock
import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import com.fatboyindustrial.gsonjavatime.Converters
import com.google.gson.GsonBuilder
import scala.util.Try
import java.util.UUID

trait HistoryStorage {

  def loadHistory(): Seq[HistoryEntry]

  def saveEntry(entry: HistoryEntry)

}

object HistoryImpl {

  private val NotInHistory = -1

}

class HistoryImpl(
    storage: HistoryStorage,
    clock: Clock = Clock.systemDefaultZone,
    sessionId: String = UUID.randomUUID().toString()) extends History {

  import HistoryImpl._

  private var history: Seq[HistoryEntry] = storage.loadHistory()
  private var historyPos = NotInHistory

  def resetHistoryPosition() = historyPos = NotInHistory

  def goForwards(): Option[String] =
    if (historyPos >= 0) {
      historyPos -= 1
      Some(
        if (historyPos == NotInHistory)
          ""
        else
          history(historyPos).command)
    } else
      None

  def goBackwards(): Option[String] =
    if (historyPos < history.size - 1) {
      historyPos += 1
      Some(history(historyPos).command)
    } else
      None

  def record(cmd: String, commandNumber: Int, mish: Boolean) {
    val entry = HistoryEntry(sessionId, commandNumber, clock.instant, cmd, mish)
    history = entry +: history
    storage.saveEntry(entry)
  }

  def getHistory: Seq[HistoryEntry] = history.sortBy(_.timestamp).reverse

}