package com.github.mdr.mash.ns.os

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime.MashList

object UsersFunction extends MashFunction("os.users") {

  val userInteractions = UserInteractions.default

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashList =
    MashList(userInteractions.passwdEntries.map(UserSummaryClass.fromPasswdEntry))

  override def typeInferenceStrategy = Type.Seq(Type.Instance(UserSummaryClass))

  override def summaryOpt = Some("The users on the system")

}
