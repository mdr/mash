package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.{ Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.{ NumberClass, StringClass }
import com.github.mdr.mash.os.{ GroupEntry, PasswdEntry, UserInteractions }
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap

object GroupInfoClass extends MashClass("os.GroupInfo") {

  private val userInteractions = UserInteractions.default

  object Fields {
    lazy val Name = Field("name", Some("Name of the group"), StringClass taggedWith GroupClass)
    lazy val Gid = Field("gid", Some("Id of the group"), NumberClass taggedWith GidClass)
    lazy val Users = Field("users", Some("Users in the group"), Type.Seq(StringClass taggedWith UsernameClass))
  }

  import Fields._

  override val fields = Seq(Name, Gid, Users)

  override val staticMethods = Seq(NewStaticMethod(this))

  def makeGroupInfo(groupEntry: GroupEntry, passwdEntries: Seq[PasswdEntry]): MashObject = {
    val users = GroupClass.UsersMethod.getUsers(groupEntry)
    MashObject.of(
      ListMap(
        Fields.Name -> MashString(groupEntry.group, GroupClass),
        Fields.Gid -> MashNumber(groupEntry.gid, GidClass),
        Fields.Users -> MashList(users)),
      GroupInfoClass)
  }

  override def summaryOpt = Some("A user group")

}