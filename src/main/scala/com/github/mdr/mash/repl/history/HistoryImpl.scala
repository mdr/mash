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
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashValue

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
    sessionId: UUID = UUID.randomUUID) extends History {

  import HistoryImpl._

  private var history: Seq[HistoryEntry] = storage.loadHistory()
  private var historyPos = NotInHistory
  private var inProgressCommandOpt: Option[String] = None

  def resetHistoryPosition() = {
    historyPos = NotInHistory
    inProgressCommandOpt = None
  }

  def forgetInProgressCommand() {
    inProgressCommandOpt = None
  }

  def goForwards(): Option[String] =
    if (historyPos >= 0) {
      historyPos -= 1
      if (historyPos == NotInHistory) {
        val result = inProgressCommandOpt
        inProgressCommandOpt = None
        result
      } else
        Some(history(historyPos).command)
    } else
      None

  def goBackwards(inProgressCommand: String): Option[String] = {
    if (historyPos == NotInHistory)
      inProgressCommandOpt = Some(inProgressCommand)
    if (historyPos < history.size - 1) {
      historyPos += 1
      Some(history(historyPos).command)
    } else
      None
  }

  def record(cmd: String, commandNumber: Int, mish: Boolean, resultOpt: Option[MashValue], workingDirectory: Path) {
    val entry = HistoryEntry(sessionId, commandNumber, clock.instant, cmd, mish, resultOpt.getOrElse(MashNull), workingDirectory.toString)
    history = entry +: history
    storage.saveEntry(entry.copy(result = null))
  }

  def getHistory: Seq[HistoryEntry] = history.sortBy(_.timestamp).reverse

}