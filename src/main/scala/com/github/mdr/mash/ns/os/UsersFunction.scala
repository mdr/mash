package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantTypeInferenceStrategy, Type }
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime.MashList

object UsersFunction extends MashFunction("os.users") {

  val userInteractions = UserInteractions.default

  val params = ParameterModel()

  def apply(arguments: Arguments): MashList = {
    params.validate(arguments)
    MashList(userInteractions.passwdEntries.map(UserSummaryClass.fromPasswdEntry))
  }

  override def typeInferenceStrategy = Type.Seq(Type.Instance(UserSummaryClass))

  override def summaryOpt = Some("The users on the system")

}
