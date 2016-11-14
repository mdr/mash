package com.github.mdr.mash.repl

import com.github.mdr.mash.repl.history.{ HistoryEntry, HistoryStorage }

class InMemoryHistoryStorage(initialEntries: Seq[HistoryEntry] = Seq()) extends HistoryStorage {

  private var entries: Seq[HistoryEntry] = Seq()

  def loadHistory(): Seq[HistoryEntry] = entries

  def saveEntry(entry: HistoryEntry) { entries :+= entry }

}