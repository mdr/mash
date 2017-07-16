package com.github.mdr.mash.ns.git

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.git.LogFunction.asCommitObject
import com.github.mdr.mash.runtime.{ MashBoolean, MashObject }

object CommitFunction extends MashFunction("git.commit") {

  object Params {
    val Message = Parameter(
      nameOpt = Some("message"),
      summaryOpt = Some("Message"))
    val All = Parameter(
      nameOpt = Some("all"),
      summaryOpt = Some("Automatically stage files that have been modified and deleted, but new files are not affected (default false)"),
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isBooleanFlag = true)
    val Amend = Parameter(
      nameOpt = Some("amend"),
      summaryOpt = Some("Replace the tip of the current branch by creating a new commit (default false)"),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Message, All, Amend)

  def call(boundParams: BoundParams): MashObject = {
    val all = boundParams(All).isTruthy
    val amend = boundParams(Amend).isTruthy
    val message = boundParams.validateString(Message).s
    GitHelper.withGit { git ⇒
      val newCommit = git.commit.setMessage(message).setAll(all).setAmend(amend).call()
      asCommitObject(newCommit)
    }
  }

  override def typeInferenceStrategy = CommitClass

  override def summaryOpt = Some("Record changes to the repository")

}