package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type }
import com.github.mdr.mash.ns.core.{ AnyClass, NumberClass, StringClass }
import com.github.mdr.mash.os.{ GroupEntry, UserInteractions }
import com.github.mdr.mash.runtime._

object GroupClass extends MashClass("os.Group") {

  private val userInteractions = UserInteractions.default

  override val methods = Seq(
    GidMethod,
    UsersMethod)

  object GidMethod extends MashMethod(GroupInfoClass.Fields.Gid.name) {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      val group = target.asInstanceOf[MashString].s
      val groupEntryOpt = userInteractions.groupEntries.find(_.group == group)
      groupEntryOpt.map(entry ⇒ MashNumber(entry.gid, GidClass)).getOrElse(MashNull)
    }

    override def typeInferenceStrategy = Type.Tagged(NumberClass, GidClass)

    override def summaryOpt = Some("Id of this group (GID)")

  }

  object UsersMethod extends MashMethod(GroupInfoClass.Fields.Users.name) {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      val group = target.asInstanceOf[MashString].s
      MashList(getUsers(group))
    }

    def getUsers(group: String): Seq[MashString] = {
      val groupEntryOpt = userInteractions.groupEntries.find(_.group == group)
      val passwdEntries = userInteractions.passwdEntries
      groupEntryOpt.map(getUsers(_)).getOrElse(Seq())
    }

    def getUsers(groupEntry: GroupEntry): Seq[MashString] = {
      val primaryUsers =
        for (passwdEntry ← userInteractions.passwdEntries if passwdEntry.gid == groupEntry.gid)
          yield MashString(passwdEntry.username, UsernameClass)
      val secondaryUsers = groupEntry.users.map(user ⇒ MashString(user, UsernameClass))
      primaryUsers ++ secondaryUsers
    }

    override def typeInferenceStrategy =
      Type.Seq(Type.Tagged(StringClass, UsernameClass))

    override def summaryOpt = Some("Users within this group")

  }

  override def enumerationValues: Option[Seq[String]] = Some(userInteractions.groupEntries.map(_.group).sorted)

  override def summaryOpt = Some("A tag class for user group names")

  override def parentOpt = Some(AnyClass)

}