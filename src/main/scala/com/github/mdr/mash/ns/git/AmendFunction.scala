package com.github.mdr.mash.ns.git

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.NoArgFunction.NoArgValue
import com.github.mdr.mash.runtime.MashObject

object AmendFunction extends MashFunction("git.amend") {

  import CommitFunction.Params.All

  object Params {
    val Message = Parameter(
      nameOpt = Some("message"),
      summaryOpt = Some("Message (defaults to last commit message)"),
      defaultValueGeneratorOpt = Some(NoArgValue))
  }

  import Params._

  val params = ParameterModel(Message, All)

  def call(boundParams: BoundParams): MashObject = {
    val all = boundParams(All).isTruthy
    val messageOpt = boundParams.validateStringOpt(Message).map(_.s)
    CommitFunction.doCommit(all = all, amend = true, messageOpt)
  }

  override def typeInferenceStrategy = CommitClass

  override def summaryOpt = Some("Amend the previous commit")

}