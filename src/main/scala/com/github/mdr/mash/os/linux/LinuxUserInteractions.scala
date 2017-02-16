package com.github.mdr.mash.os.linux

import java.io.File
import java.nio.charset.StandardCharsets

import com.github.mdr.mash.os.{ GroupEntry, PasswdEntry, UserInteractions }
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._

object LinuxUserInteractions extends UserInteractions {

  override def passwdEntries: Seq[PasswdEntry] = {
    def parseLine(l: String) = {
      val entries = l.split(":")
      PasswdEntry(entries(0), entries(2).toInt, entries(3).toInt, entries(4), entries(5), entries(6))
    }
    FileUtils.readLines(new File("/etc/passwd"), StandardCharsets.UTF_8).asScala.filterNot(_ startsWith "#").map(parseLine)
  }

  override def groupEntries: Seq[GroupEntry] = {
    def parseLine(l: String) = {
      val entries = l.split(":", -1)
      val users = entries(3).split(",").toSeq match {
        case Seq("") ⇒ Seq()
        case users   ⇒ users
      }
      GroupEntry(entries(0), entries(2).toInt, users)
    }
    FileUtils.readLines(new File("/etc/group"), StandardCharsets.UTF_8).asScala.filterNot(_ startsWith "#").map(parseLine)
  }

  override def currentUsername: String = System.getProperty("user.name")

}

