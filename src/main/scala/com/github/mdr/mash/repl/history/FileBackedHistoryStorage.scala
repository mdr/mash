package com.github.mdr.mash.repl.history

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths }
import java.time.Instant
import java.util.UUID

import com.fatboyindustrial.gsonjavatime.Converters
import com.github.mdr.mash.Mash
import com.github.mdr.mash.runtime.MashNull
import com.google.gson.GsonBuilder
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.util.Try

object FileBackedHistoryStorage {

  private val HistoryFile = Mash.MashDir.resolve("history")

}

class FileBackedHistoryStorage extends HistoryStorage {

  import FileBackedHistoryStorage._

  private val gson = Converters.registerAll(new GsonBuilder).create()

  def loadHistory(): Seq[HistoryEntry] =
    if (Files.exists(HistoryFile)) {
      val lines = FileUtils.readLines(HistoryFile.toFile, StandardCharsets.UTF_8).asScala
      lines.flatMap(loadHistoryEntry).sortBy(_.timestamp).reverse
    } else
      Seq()

  private def loadHistoryEntry(s: String): Option[HistoryEntry] =
    Try(gson.fromJson(s, classOf[StoredHistoryEntry])).toOption.map(e ⇒
      HistoryEntry(e.sessionId, e.commandNumber, e.timestamp, e.command, e.mish, result = MashNull, Paths.get(e.workingDirectory)))

  def saveEntry(entry: HistoryEntry) {
    val storedEntry = StoredHistoryEntry(entry.sessionId, entry.commandNumber, entry.timestamp, entry.command, entry.mish, entry.workingDirectory.toString)
    Mash.ensureMashDirExists()
    val json = gson.toJson(storedEntry)
    FileUtils.writeLines(HistoryFile.toFile, Seq(json).asJava, true)
  }

}

case class StoredHistoryEntry(sessionId: UUID,
                              commandNumber: Int,
                              timestamp: Instant,
                              command: String,
                              mish: Boolean,
                              workingDirectory: String)
