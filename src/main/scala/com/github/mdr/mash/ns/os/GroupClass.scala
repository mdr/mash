package com.github.mdr.mash.ns.os

import java.io.File
import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.os.linux.LinuxUserInteractions
import com.github.mdr.mash.os.GroupEntry
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashValue

object GroupClass extends MashClass("os.Group") {

  private val userInteractions = UserInteractions.default

  override val methods = Seq(
    GidMethod,
    UsersMethod)

  object GidMethod extends MashMethod(GroupInfoClass.Fields.Gid.name) {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashValue = {
      params.validate(arguments)
      val group = target.asInstanceOf[MashString].s
      val groupEntryOpt = userInteractions.groupEntries.find(_.group == group)
      groupEntryOpt.map(entry ⇒ MashNumber(entry.gid, GidClass)).getOrElse(MashNull)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(NumberClass, GidClass))

    override def summary = "Id of this group (GID)"

  }

  object UsersMethod extends MashMethod(GroupInfoClass.Fields.Users.name) {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashList = {
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
      ConstantMethodTypeInferenceStrategy(Type.Seq(Type.Tagged(StringClass, UsernameClass)))

    override def summary = "Users within this group"

  }

  override def enumerationValues: Option[Seq[String]] = Some(userInteractions.groupEntries.map(_.group).sorted)

  override def summary = "A tag class for user group names"

}