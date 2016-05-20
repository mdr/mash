package com.github.mdr.mash

import com.github.mdr.mash.repl.History
import com.github.mdr.mash.repl.HistoryEntry
import java.time.Clock

object MemoryHistory {

  private val NotInHistory = -1

}

class MemoryHistory extends History {

  import MemoryHistory._

  private var history: Seq[HistoryEntry] = Seq()
  private var historyPos = NotInHistory
  private val clock = Clock.systemDefaultZone

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
  }

  def getHistory: Seq[HistoryEntry] = history.sortBy(_.timestamp)

}
