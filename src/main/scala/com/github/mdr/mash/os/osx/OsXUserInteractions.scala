package com.github.mdr.mash.os.osx

import java.nio.charset.StandardCharsets

import com.github.mdr.mash.os.{ GroupEntry, PasswdEntry, UserInteractions }
import org.apache.commons.io.IOUtils

import scala.util.Try
import scala.xml.{ Node, XML }

object OsXUserInteractions extends UserInteractions {

  private def getUsersXml(): Node = runAndGetXml("dscl -plist . -readall /Users")

  private def getGroupsXml(): Node = runAndGetXml("dscl -plist . -readall /Groups")

  private def runAndGetText(cmd: String): String = {
    val process = new ProcessBuilder(cmd.split(" "): _*).redirectOutput(ProcessBuilder.Redirect.PIPE).start()
    IOUtils.toString(process.getInputStream, StandardCharsets.UTF_8)
  }

  private def runAndGetXml(cmd: String): Node = {
    val text = runAndGetText(cmd).split("\n").filterNot(_.startsWith("<!DOCTYPE")).mkString("\n")
    XML.loadString(text)
  }

  override def passwdEntries: Seq[PasswdEntry] =
    for {
      userXml ← getUsersXml \ "array" \ "dict"
      entry ← parseUser(userXml)
    } yield entry

  private def parseDict(xml: Node): Map[String, String] = {
    var userData: Map[String, String] = Map()
    var keyTextOpt: Option[String] = None
    for (child ← xml.child) {
      if (child.label == "key") {
        keyTextOpt = Some(child.text)
      } else if (child.label == "array") {
        for (key ← keyTextOpt)
          for (c ← child.child) {
            if (c.label == "string") {
              userData += key -> c.text
            }
          }
        keyTextOpt = None
      }
    }
    userData
  }

  private def parseUser(userXml: Node): Option[PasswdEntry] = {
    val d = parseDict(userXml)
    UserWrapper(d).entryOpt
  }

  override def groupEntries: Seq[GroupEntry] = {
    val text = runAndGetText("dscacheutil -q group")
    val lines = text.split("\n").toSeq
    var data: Map[String, String] = Map()
    var groups: Seq[GroupEntry] = Seq()
    for (line ← lines) {
      if (line.isEmpty) {
        for {
          group ← data.get("name")
          gidString ← data.get("gid")
          gid ← Try(gidString.toLong).toOption
          usersString ← data.get("users")
          users = data.get("users").map(_.split(" ").toSeq).getOrElse(Seq())
        } groups :+= GroupEntry(group, gid, users)
        data = Map()
      } else {
        if (line.contains(": ")) {
          val (key, value) = line.splitAt(line.indexOf(':'))
          data += key -> value.drop(2)
        }
      }
    }
    groups
  }

  override def currentUsername: String = System.getProperty("user.name")
}

case class GroupWrapper(groupData: Map[String, String]) {

  def get(key: String) = groupData.get(key)

  def gid = get("dsAttrTypeStandard:PrimaryGroupID")

  def name = get("dsAttrTypeStandard:RealName")

}

case class UserWrapper(userData: Map[String, String]) {

  def get(key: String) = userData.get(key)

  def username = get("dsAttrTypeStandard:RecordName")

  def fullName = get("dsAttrTypeStandard:RealName")

  def gid = get("dsAttrTypeStandard:PrimaryGroupID")

  def homeDirectory = get("dsAttrTypeStandard:NFSHomeDirectory")

  def shell = get("dsAttrTypeStandard:UserShell")

  def uid = get("dsAttrTypeStandard:UniqueID")

  def entryOpt =
    for {
      username ← username
      gecos ← fullName
      uid ← uid.map(_.toInt)
      gid ← gid.map(_.toInt)
      homeDirectory ← homeDirectory
      shell ← shell
    } yield PasswdEntry(username, uid, gid, gecos, homeDirectory, shell)

}