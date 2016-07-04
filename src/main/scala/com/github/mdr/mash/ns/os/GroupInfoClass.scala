package com.github.mdr.mash.ns.os

import scala.collection.immutable.ListMap

import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.GroupEntry
import com.github.mdr.mash.os.PasswdEntry
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime._

object GroupInfoClass extends MashClass("os.GroupInfo") {

  private val userInteractions = UserInteractions.default

  object Fields {
    lazy val Name = Field("name", "Name of the group", Type.Tagged(StringClass, GroupClass))
    lazy val Gid = Field("gid", "Id of the group", Type.Tagged(NumberClass, GidClass))
    lazy val Users = Field("users", "Users in the group", Type.Seq(Type.Tagged(StringClass, UsernameClass)))
  }

  import Fields._

  override val fields = Seq(Name, Gid, Users)

  def makeGroupInfo(groupEntry: GroupEntry, passwdEntries: Seq[PasswdEntry]): MashObject = {
    val users = GroupClass.UsersMethod.getUsers(groupEntry)
    MashObject.of(
      ListMap(
        Fields.Name -> MashString(groupEntry.group, GroupClass),
        Fields.Gid -> MashNumber(groupEntry.gid, GidClass),
        Fields.Users -> MashList(users)),
      GroupInfoClass)
  }

  override def summary = "A user group"

}