package com.github.mdr.mash.os

import com.github.mdr.mash.os.osx.OsXUserInteractions
import com.github.mdr.mash.os.linux.LinuxUserInteractions
import org.apache.commons.lang3.SystemUtils

case class PasswdEntry(username: String, uid: Int, gid: Int, gecos: String, homeDirectory: String, shell: String) {

  def fullNameOpt: Option[String] = gecos.split(",").headOption

}

case class GroupEntry(group: String, gid: Long, users: Seq[String])

object UserInteractions {
  
  def default: UserInteractions = if (SystemUtils.IS_OS_MAC_OSX) OsXUserInteractions else LinuxUserInteractions
  
}

trait UserInteractions {

  def passwdEntries: Seq[PasswdEntry]

  def groupEntries: Seq[GroupEntry]

  def currentUsername: String 
  
}