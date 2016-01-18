package com.github.mdr.mash.ns.os

import scala.collection.JavaConverters._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.os.linux.LinuxUserInteractions

object GidClass extends MashClass("os.Gid") {

  private val userInteractions = LinuxUserInteractions

  override val methods = Seq(
    NameMethod,
    UsersMethod)

  object UsersMethod extends MashMethod("users") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): Seq[MashString] = {
      params.validate(arguments)
      val gid = target.asInstanceOf[MashNumber].asInt.get
      val groupEntry = userInteractions.groupEntries.find(_.gid == gid).getOrElse(
        throw new EvaluatorException(s"Could not find group with gid $gid"))
      GroupClass.UsersMethod.getUsers(groupEntry.group)
    }

    override def typeInferenceStrategy = GroupClass.UsersMethod.typeInferenceStrategy

    override def summary = GroupClass.UsersMethod.summary

  }

  object NameMethod extends MashMethod("name") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      val gid = target.asInstanceOf[MashNumber].asInt.get
      val groupEntry = userInteractions.groupEntries.find(_.gid == gid).getOrElse(
        throw new EvaluatorException(s"Could not find group with gid $gid"))
      MashString(groupEntry.group, Some(GroupClass))
    }

    override def typeInferenceStrategy =
      ConstantMethodTypeInferenceStrategy(Type.Seq(Type.Tagged(StringClass, UsernameClass)))

    override def summary = "Name of the group with this id"

  }

  override def summary = "Tag class for a group ID (GID)"

}