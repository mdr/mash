package com.github.mdr.mash.repl.history

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.collection.JavaConverters._
import scala.util.Try

import org.apache.commons.io.FileUtils

import com.fatboyindustrial.gsonjavatime.Converters
import com.github.mdr.mash.Mash
import com.google.gson.GsonBuilder

object FileBackedHistoryStorage {
  
  private val HistoryFile = Mash.MashDir.resolve("history")
  
}

class FileBackedHistoryStorage extends HistoryStorage {
  
  import FileBackedHistoryStorage._
  
  private val gson = Converters.registerAll(new GsonBuilder()).create()

  def loadHistory(): Seq[HistoryEntry] =
    if (Files.exists(HistoryFile)) {
      val lines = FileUtils.readLines(HistoryFile.toFile, StandardCharsets.UTF_8).asScala
      lines.flatMap(loadHistoryEntry).sortBy(_.timestamp).reverse
    } else
      Seq()

  private def loadHistoryEntry(s: String): Option[HistoryEntry] =
    Try(gson.fromJson(s, classOf[HistoryEntry])).toOption

  def saveEntry(entry: HistoryEntry) {
    Mash.ensureMashDirExists()
    val json = gson.toJson(entry)
    FileUtils.writeLines(HistoryFile.toFile, Seq(json).asJava, true)
  }
}
