package com.github.mdr.mash.repl.history

import java.nio.file.Paths
import java.time.{ Clock, Instant, ZoneId }

object InMemoryHistoryStorage {

  private object MonotonicallyTickingClock extends Clock {
    private val dummyClock = Clock.systemDefaultZone
    private var now = dummyClock.instant()

    override def getZone: ZoneId = dummyClock.getZone

    override def instant(): Instant = {
      now = now.plusSeconds(1)
      now
    }

    override def withZone(zone: ZoneId): Clock = dummyClock.withZone(zone)
  }

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