package com.github.mdr.mash.repl.history

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

import scala.collection.JavaConverters._
import scala.util.Try

import org.apache.commons.io.FileUtils

import com.fatboyindustrial.gsonjavatime.Converters
import com.google.gson.GsonBuilder

class FileBackedHistoryStorage extends HistoryStorage {

  private val historyFile: Path = History.MashDir.resolve("history")

  private val gson = Converters.registerAll(new GsonBuilder()).create()

  def loadHistory(): Seq[HistoryEntry] =
    if (Files.exists(historyFile)) {
      val lines = FileUtils.readLines(historyFile.toFile, StandardCharsets.UTF_8).asScala
      lines.flatMap(loadHistoryEntry).sortBy(_.timestamp).reverse
    } else
      Seq()

  private def loadHistoryEntry(s: String): Option[HistoryEntry] =
    Try(gson.fromJson(s, classOf[HistoryEntry])).toOption

  def saveEntry(entry: HistoryEntry) {
    if (!Files.exists(History.MashDir))
      Files.createDirectory(History.MashDir)
    val json = gson.toJson(entry)
    FileUtils.writeLines(historyFile.toFile, Seq(json).asJava, true)
  }
}
