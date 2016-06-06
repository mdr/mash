package com.github.mdr.mash.ns.os

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.os.PasswdEntry
import com.github.mdr.mash.os.linux.LinuxUserInteractions
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime.MashList

object UsersFunction extends MashFunction("os.users") {

  val userInteractions = UserInteractions.default

  val params = ParameterModel()

  def apply(arguments: Arguments): MashList = {
    params.validate(arguments)
    MashList(userInteractions.passwdEntries.map(UserSummaryClass.fromPasswdEntry))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(UserSummaryClass)))

  override def summary = "The users on the system"

}
