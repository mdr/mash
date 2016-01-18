package com.github.mdr.mash.os

case class PasswdEntry(username: String, uid: Int, gid: Int, gecos: String, homeDirectory: String, shell: String) {

  def fullNameOpt: Option[String] = gecos.split(",").headOption

}

case class GroupEntry(group: String, gid: Int, users: Seq[String])

trait UserInteractions {

  def passwdEntries: Seq[PasswdEntry]

  def groupEntries: Seq[GroupEntry]

  def currentUsername: String 
  
}