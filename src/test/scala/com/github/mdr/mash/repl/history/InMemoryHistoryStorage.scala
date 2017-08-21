package com.github.mdr.mash.repl.history

import java.nio.file.{ Path, Paths }

import com.github.mdr.mash.utils.MonotonicallyTickingClock

object InMemoryHistoryStorage {

  val WorkingDirectory = Paths.get("")

  def testHistoryWithPaths(entries: (String, Path)*): HistoryImpl = {
    val history = new HistoryImpl(new InMemoryHistoryStorage(), clock = MonotonicallyTickingClock)
    for (((command, path), i) ← entries.zipWithIndex)
      history.record(command, i, workingDirectory = path)
    history
  }

  def testHistory(entries: String*): HistoryImpl =
    testHistoryWithPaths(entries.map(_ → WorkingDirectory): _*)

}

class InMemoryHistoryStorage(initialEntries: Seq[HistoryEntry] = Seq()) extends HistoryStorage {

  private var entries: Seq[HistoryEntry] = Seq()

  def loadHistory(): Seq[HistoryEntry] = entries

  def saveEntry(entry: HistoryEntry) {
    entries :+= entry
  }

}