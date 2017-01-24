package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantTypeInferenceStrategy, Type }
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime.MashList

object GroupsFunction extends MashFunction("os.groups") {

  private val userInteractions = UserInteractions.default

  val params = ParameterModel()

  def apply(arguments: Arguments): MashList = {
    params.validate(arguments)
    val passwdEntries = userInteractions.passwdEntries
    MashList(userInteractions.groupEntries.map(GroupInfoClass.makeGroupInfo(_, passwdEntries)))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(GroupInfoClass)))

  override def summaryOpt = Some("User groups on the system")

}
