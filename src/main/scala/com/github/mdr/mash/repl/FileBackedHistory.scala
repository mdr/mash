package com.github.mdr.mash.repl

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.time.Clock

import scala.collection.JavaConverters._

import org.apache.commons.io.FileUtils

import com.fatboyindustrial.gsonjavatime.Converters
import com.google.gson.GsonBuilder

object FileBackedHistory {

  private val NotInHistory = -1

  private val historyFile: Path = History.MashDir.resolve("history")

}

class FileBackedHistory extends History {

  import FileBackedHistory._

  private val clock: Clock = Clock.systemDefaultZone
  private val gson = Converters.registerAll(new GsonBuilder()).create()

  private var history: Seq[HistoryEntry] = loadHistory()
  private var historyPos = NotInHistory

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
    if (Files.exists(historyFile))
      FileUtils.readLines(historyFile.toFile, StandardCharsets.UTF_8).asScala.reverse.map(loadHistoryEntry)
    else
      Seq()

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
    if (!Files.exists(History.MashDir))
      Files.createDirectory(History.MashDir)
    val json = gson.toJson(entry)
    FileUtils.writeLines(historyFile.toFile, Seq(json).asJava, true)
  }

  def getHistory: Seq[HistoryEntry] = history.sortBy(_.timestamp).reverse

}