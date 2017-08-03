package com.github.mdr.mash.repl.history

import java.nio.file.Path
import java.time.Clock
import java.util.UUID

import com.github.mdr.mash.runtime.{ MashNull, MashValue }

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
  private var isCommitted: Boolean = true

  def resetHistoryPosition() = {
    historyPos = NotInHistory
    inProgressCommandOpt = None
  }

  def forgetInProgressCommand() {
    inProgressCommandOpt = None
  }

  def goForwards(): Option[String] =
    if (historyPos >= 0) {
      isCommitted = false
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
      isCommitted = false
      historyPos += 1
      Some(history(historyPos).command)
    } else
      None
  }

  def record(cmd: String, commandNumber: Int, mish: Boolean, resultOpt: Option[MashValue], workingDirectory: Path) {
    val result = resultOpt.getOrElse(MashNull)
    val entry = HistoryEntry(sessionId, commandNumber, clock.instant, cmd, mish, result, workingDirectory)
    history = entry +: history
    storage.saveEntry(entry)
    isCommitted = true
  }

  def getHistory: Seq[HistoryEntry] = history.sortBy(_.timestamp).reverse

  override def commitToEntry() {
    isCommitted = true
  }

  override def isCommittedToEntry: Boolean = isCommitted

}