package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime.MashList

object GroupsFunction extends MashFunction("os.groups") {

  private val userInteractions = UserInteractions.default

  val params = ParameterModel()

  def apply(boundParams: BoundParams): MashList = {
    val passwdEntries = userInteractions.passwdEntries
    MashList(userInteractions.groupEntries.map(GroupInfoClass.makeGroupInfo(_, passwdEntries)))
  }

  override def typeInferenceStrategy = Type.Seq(Type.Instance(GroupInfoClass))

  override def summaryOpt = Some("User groups on the system")

}
