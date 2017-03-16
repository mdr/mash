package com.github.mdr.mash.ns.git

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashBoolean, MashUnit }

object CommitFunction extends MashFunction("git.commit") {

  private val filesystem = LinuxFileSystem

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

  val params = ParameterModel(Seq(Message, All, Amend))

  def apply(boundParams: BoundParams): MashUnit = {
    val all = boundParams(All).isTruthy
    val amend = boundParams(Amend).isTruthy
    val message = boundParams.validateString(Message).s
    GitHelper.withGit { git ⇒
      git.commit.setMessage(message).setAll(all).setAmend(amend).call()
    }
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Record changes to the repository")

}