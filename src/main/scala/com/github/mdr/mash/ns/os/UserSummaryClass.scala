package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.{ PasswdEntry, UserInteractions }
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap

object UserSummaryClass extends MashClass("os.UserSummary") {

  private val userInteractions = UserInteractions.default

  object Fields {
    val Name = Field("name", Some("Name of user"), StringClass taggedWith UsernameClass)
    val Uid = Field("uid", Some("Id of user (UID)"), StringClass taggedWith UidClass)
    val PrimaryGroup = Field("primaryGroup", Some("Primary group to which the user belongs"), StringClass taggedWith GroupClass)
    val Home = Field("home", Some("Home directory of this user"), StringClass taggedWith PathClass)
    val Shell = Field("shell", Some("Shell used by this user"), StringClass taggedWith PathClass)
  }

  import Fields._

  override val fields = Seq(Name, Uid, PrimaryGroup, Home, Shell)

  override val methods = Seq(
    FullNameMethod,
    GroupsMethod)

  override val staticMethods = Seq(NewStaticMethod(this))

  case class Wrapper(value: MashValue) extends AbstractObjectWrapper(value) {

    def username: String = getStringField(Name)

    def primaryGroup: String = getStringField(PrimaryGroup)

  }

  object FullNameMethod extends MashMethod("fullName") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val username = Wrapper(target).username
      val fullNameOpt =
        for {
          entry ← userInteractions.passwdEntries.find(_.username == username)
          fullName ← entry.fullNameOpt
        } yield MashString(fullName)
      fullNameOpt.getOrElse(MashNull)
    }

    override def typeInferenceStrategy = StringClass

    override def summaryOpt = Some("Full name of this user")

  }

  object GroupsMethod extends MashMethod("groups") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashList = {
      val user = Wrapper(target)
      val primaryGroup = user.primaryGroup
      val username = user.username
      val secondaryGroups =
        userInteractions.groupEntries
          .filter(_.users contains username)
          .map(_.group)
      MashList((primaryGroup +: secondaryGroups).map(makeGroup))
    }

    private def makeGroup(group: String) = MashString(group, Some(GroupClass))

    override def typeInferenceStrategy = Type.Seq(StringClass taggedWith GroupClass)

    override def summaryOpt = Some("Groups this user is a member of")

  }

  def fromPasswdEntry(entry: PasswdEntry): MashObject = {
    val username = MashString(entry.username, UsernameClass)
    val group = userInteractions.groupEntries
      .find(_.gid == entry.gid)
      .map(ge ⇒ MashString(ge.group, GroupClass))
      .getOrElse(MashNull)
    val uid = MashNumber(entry.uid, UidClass)
    val home = MashString(entry.homeDirectory, PathClass)
    val shell = MashString(entry.shell, PathClass)
    MashObject.of(
      ListMap(
        Name -> username,
        Uid -> uid,
        PrimaryGroup -> group,
        Home -> home,
        Shell -> shell),
      UserSummaryClass)
  }

  override def summaryOpt = Some("A user")

}

