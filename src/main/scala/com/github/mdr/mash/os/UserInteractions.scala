package com.github.mdr.mash.os

import com.github.mdr.mash.os.osx.OsXUserInteractions

case class PasswdEntry(username: String, uid: Int, gid: Int, gecos: String, homeDirectory: String, shell: String) {

  def fullNameOpt: Option[String] = gecos.split(",").headOption

}

case class GroupEntry(group: String, gid: Long, users: Seq[String])

object UserInteractions {
  
  def default: UserInteractions = OsXUserInteractions
  
}

trait UserInteractions {

  def passwdEntries: Seq[PasswdEntry]

  def groupEntries: Seq[GroupEntry]

  def currentUsername: String 
  
}