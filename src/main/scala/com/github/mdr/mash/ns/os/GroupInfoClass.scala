package com.github.mdr.mash.ns.os

import scala.collection.JavaConverters._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.os.linux.LinuxUserInteractions
import scala.collection.immutable.ListMap
import com.github.mdr.mash.os.PasswdEntry
import com.github.mdr.mash.os.GroupEntry

object GroupInfoClass extends MashClass("os.GroupInfo") {

  private val userInteractions = LinuxUserInteractions

  object Fields {
    lazy val Name = Field("name", "Name of the group", Type.Tagged(StringClass, GroupClass))
    lazy val Gid = Field("gid", "Id of the group", Type.Tagged(NumberClass, GidClass))
    lazy val Users = Field("users", "Users in the group", Type.Seq(Type.Tagged(StringClass, UsernameClass)))
  }

  import Fields._

  override val fields = Seq(Name, Gid, Users)

  def makeGroupInfo(groupEntry: GroupEntry, passwdEntries: Seq[PasswdEntry]): MashObject = {
    val primaryUsers = userInteractions.passwdEntries.filter(_.gid == groupEntry.gid).map(user ⇒ MashString(user.username, Some(UsernameClass)))
    val secondaryUsers = groupEntry.users.map(user ⇒ MashString(user, Some(UsernameClass)))
    val users = primaryUsers ++ secondaryUsers
    MashObject(
      ListMap(
        Fields.Name -> MashString(groupEntry.group, Some(GroupClass)),
        Fields.Gid -> MashNumber(groupEntry.gid, Some(GidClass)),
        Fields.Users -> MashList(users)),
      GroupInfoClass)
  }

  override def summary = "A user group"

}