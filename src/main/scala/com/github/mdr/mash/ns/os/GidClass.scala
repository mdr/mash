package com.github.mdr.mash.ns.os

import scala.collection.JavaConverters._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.os.linux.LinuxUserInteractions
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashValue

object GidClass extends MashClass("os.Gid") {

  private val userInteractions = UserInteractions.default

  override val methods = Seq(
    NameMethod,
    UsersMethod)

  object UsersMethod extends MashMethod("users") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      val gid = target.asInstanceOf[MashNumber].asInt.get
      val groupEntry = userInteractions.groupEntries.find(_.gid == gid).getOrElse(
        throw new EvaluatorException(s"Could not find group with gid $gid"))
      MashList(GroupClass.UsersMethod.getUsers(groupEntry.group))
    }

    override def typeInferenceStrategy = GroupClass.UsersMethod.typeInferenceStrategy

    override def summary = GroupClass.UsersMethod.summary

  }

  object NameMethod extends MashMethod("name") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      val gid = target.asInstanceOf[MashNumber].asInt.get
      val groupEntry = userInteractions.groupEntries.find(_.gid == gid).getOrElse(
        throw new EvaluatorException(s"Could not find group with gid $gid"))
      MashString(groupEntry.group, GroupClass)
    }

    override def typeInferenceStrategy =
      ConstantMethodTypeInferenceStrategy(Type.Seq(Type.Tagged(StringClass, UsernameClass)))

    override def summary = "Name of the group with this id"

  }

  override def summary = "Tag class for a group ID (GID)"

  override def parentOpt = Some(AnyClass)

}