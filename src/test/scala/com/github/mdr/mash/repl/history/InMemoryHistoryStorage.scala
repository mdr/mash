package com.github.mdr.mash.repl.history

import java.nio.file.Paths

import com.github.mdr.mash.utils.MonotonicallyTickingClock

object InMemoryHistoryStorage {

  def testHistory(entries: String*): HistoryImpl = {
    val history = new HistoryImpl(new InMemoryHistoryStorage(), clock = MonotonicallyTickingClock)
    for ((entry, i) ← entries.zipWithIndex)
      history.record(entry, i, mish = false, resultOpt = None, workingDirectory = Paths.get(""))
    history
  }

}

class InMemoryHistoryStorage(initialEntries: Seq[HistoryEntry] = Seq()) extends HistoryStorage {

  private var entries: Seq[HistoryEntry] = Seq()

  def loadHistory(): Seq[HistoryEntry] = entries

  def saveEntry(entry: HistoryEntry) {
    entries :+= entry
  }

}