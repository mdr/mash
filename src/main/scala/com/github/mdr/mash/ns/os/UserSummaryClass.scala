package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type }
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

  case class Wrapper(target: MashValue) {

    private val user = target.asInstanceOf[MashObject]

    def username: String = user(Name).asInstanceOf[MashString].s

    def primaryGroup: MashString = user(PrimaryGroup).asInstanceOf[MashString]

  }

  object FullNameMethod extends MashMethod("fullName") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
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

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      val user = Wrapper(target)
      val primaryGroup = user.primaryGroup
      val username = user.username
      val secondaryGroups = userInteractions.groupEntries.filter(_.users.contains(username)).map(entry ⇒ MashString(entry.group, Some(GroupClass)))
      MashList(primaryGroup +: secondaryGroups)
    }

    override def typeInferenceStrategy = Type.Seq(Type.Tagged(StringClass, GroupClass))

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

