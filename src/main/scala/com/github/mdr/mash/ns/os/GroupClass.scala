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

object GroupClass extends MashClass("os.Group") {

  private val userInteractions = LinuxUserInteractions

  override val methods = Seq(
    GidMethod,
    UsersMethod)

  object GidMethod extends MashMethod(GroupInfoClass.Fields.Gid.name) {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): Any = {
      params.validate(arguments)
      val group = target.asInstanceOf[MashString].s
      val groupEntryOpt = userInteractions.groupEntries.find(_.group == group)
      groupEntryOpt.map(entry ⇒ MashNumber(entry.gid, Some(GidClass))).orNull
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

    def getUsers(group: String) = {
      val groupEntryOpt = userInteractions.groupEntries.find(_.group == group)
      val passwdEntries = userInteractions.passwdEntries
      groupEntryOpt.map { groupEntry ⇒
        val primaryUsers = passwdEntries.filter(_.gid == groupEntry.gid).map(_.username).map(user ⇒ MashString(user, Some(UsernameClass)))
        val secondaryUsers = groupEntry.users.map(user ⇒ MashString(user, Some(UsernameClass)))
        primaryUsers ++ secondaryUsers
      }.getOrElse(Seq())
    }

    override def typeInferenceStrategy =
      ConstantMethodTypeInferenceStrategy(Type.Seq(Type.Tagged(StringClass, UsernameClass)))

    override def summary = "Users within this group"

  }

  override def enumerationValues: Option[Seq[String]] = Some(userInteractions.groupEntries.map(_.group).sorted)

  override def summary = "A tag class for user group names"

}