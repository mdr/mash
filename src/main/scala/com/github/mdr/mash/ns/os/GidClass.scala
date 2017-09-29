package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime.{ MashList, MashNumber, MashString, MashValue }

object GidClass extends MashClass("os.Gid") {

  private val userInteractions = UserInteractions.default

  override val methods = Seq(
    NameMethod,
    UsersMethod)

  object UsersMethod extends MashMethod("users") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashList = {
      val gid = target.asInstanceOf[MashNumber].asInt.get
      val groupEntry = userInteractions.groupEntries.find(_.gid == gid).getOrElse(
        throw EvaluatorException(s"Could not find group with gid $gid"))
      MashList(GroupClass.UsersMethod.getUsers(groupEntry.group))
    }

    override def typeInferenceStrategy = GroupClass.UsersMethod.typeInferenceStrategy

    override def summaryOpt = GroupClass.UsersMethod.summaryOpt

  }

  object NameMethod extends MashMethod("name") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashString = {
      val gid = target.asInstanceOf[MashNumber].asInt.get
      val groupEntry = userInteractions.groupEntries.find(_.gid == gid).getOrElse(
        throw EvaluatorException(s"Could not find group with gid $gid"))
      MashString(groupEntry.group, GroupClass)
    }

    override def typeInferenceStrategy = Type.Seq(NumberClass taggedWith UsernameClass)

    override def summaryOpt = Some("Name of the group with this id")

  }

  override def summaryOpt = Some("Tag class for a group ID (GID)")

  override def parentOpt = Some(AnyClass)

}