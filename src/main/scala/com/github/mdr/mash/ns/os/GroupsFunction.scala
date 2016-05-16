package com.github.mdr.mash.ns.os

import scala.collection.JavaConverters._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.os.linux.LinuxUserInteractions
import com.github.mdr.mash.os.UserInteractions

object GroupsFunction extends MashFunction("os.groups") {

  private val userInteractions = UserInteractions.default

  val params = ParameterModel()

  def apply(arguments: Arguments): MashList = {
    params.validate(arguments)
    val passwdEntries = userInteractions.passwdEntries
    MashList(userInteractions.groupEntries.map(GroupInfoClass.makeGroupInfo(_, passwdEntries)))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(GroupInfoClass)))

  override def summary = "User groups on the system"

}
