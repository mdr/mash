package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime.MashObject

object UserFunction extends MashFunction("os.user") {

  private val userInteractions = UserInteractions.default

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashObject = {
    val username = userInteractions.currentUsername
    val passwdEntry = userInteractions.passwdEntries.find(_.username == username).getOrElse(
      throw new EvaluatorException(s"Could not find full user information for user '$username'"))
    UserSummaryClass.fromPasswdEntry(passwdEntry)
  }

  override def typeInferenceStrategy = UserSummaryClass

  override def summaryOpt = Some("The current user")

}