package com.github.mdr.mash.ns.os

import java.io.File
import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.os.linux.LinuxUserInteractions
import com.github.mdr.mash.os.PasswdEntry
import scala.collection.immutable.ListMap
import com.github.mdr.mash.os.UserInteractions

object UserSummaryClass extends MashClass("os.UserSummary") {

  private val userInteractions = UserInteractions.default

  object Fields {
    val Name = Field("name", "Name of user", Type.Tagged(StringClass, UsernameClass))
    val Uid = Field("uid", "Id of user (UID)", Type.Tagged(StringClass, UidClass))
    val PrimaryGroup = Field("primaryGroup", "Primary group to which the user belongs", Type.Tagged(StringClass, GroupClass))
    val Home = Field("home", "Home directory of this user", Type.Tagged(StringClass, PathClass))
    val Shell = Field("shell", "Shell used by this user", Type.Tagged(StringClass, PathClass))
  }

  import Fields._

  override val fields = Seq(Name, Uid, PrimaryGroup, Home, Shell)

  override val methods = Seq(
    FullNameMethod,
    GroupsMethod)

  case class Wrapper(target: Any) {
    
    private val user = target.asInstanceOf[MashObject]
    
    def username: String = user.field(Name).asInstanceOf[MashString].s

    def primaryGroup: MashString = user.field(PrimaryGroup).asInstanceOf[MashString]

  }
    
  object FullNameMethod extends MashMethod("fullName") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      val username = Wrapper(target).username
      val fullNameOpt =
        for {
          entry ← userInteractions.passwdEntries.find(_.username == username)
          fullName ← entry.fullNameOpt
        } yield MashString(fullName)
      fullNameOpt.orNull
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(StringClass))

    override def summary = "Full name of this user"

  }

  object GroupsMethod extends MashMethod("groups") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashList = {
      params.validate(arguments)
      val user = Wrapper(target)
      val primaryGroup = user.primaryGroup 
      val username = user.username
      val secondaryGroups = userInteractions.groupEntries.filter(_.users.contains(username)).map(entry ⇒ MashString(entry.group, Some(GroupClass)))
      MashList(primaryGroup +: secondaryGroups)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Seq(Type.Tagged(StringClass, GroupClass)))

    override def summary = "Groups this user is a member of"

  }

  def fromPasswdEntry(entry: PasswdEntry): MashObject = {
    val username = MashString(entry.username, UsernameClass)
    val group = userInteractions.groupEntries
      .find(_.gid == entry.gid)
      .map(ge => MashString(ge.group, GroupClass))
      .orNull
    val uid = MashNumber(entry.uid, UidClass)
    val home = MashString(entry.homeDirectory, PathClass)
    val shell = MashString(entry.shell, PathClass)
    MashObject(
      ListMap(
        Name -> username,
        Uid -> uid,
        PrimaryGroup -> group,
        Home -> home,
        Shell -> shell),
      UserSummaryClass)
  }

  override def summary = "A user"

}

