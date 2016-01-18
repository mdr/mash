package com.github.mdr.mash.os.linux

import java.io.File
import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.os.PasswdEntry
import com.github.mdr.mash.os.GroupEntry
import com.github.mdr.mash.os.UserInteractions

object LinuxUserInteractions extends UserInteractions {

  override def passwdEntries: Seq[PasswdEntry] = {
    def parseLine(l: String) = {
      val entries = l.split(":")
      PasswdEntry(entries(0), entries(2).toInt, entries(3).toInt, entries(4), entries(5), entries(6))
    }
    FileUtils.readLines(new File("/etc/passwd")).asScala.map(parseLine)
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
    FileUtils.readLines(new File("/etc/group")).asScala.map(parseLine)
  }

  override def currentUsername: String = System.getProperty("user.name")

}

