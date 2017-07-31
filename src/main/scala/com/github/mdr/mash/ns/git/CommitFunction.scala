package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.git.LogFunction.asCommitObject
import com.github.mdr.mash.runtime.MashObject
import org.eclipse.jgit.api.Git

import scala.collection.JavaConverters._

object CommitFunction extends MashFunction("git.commit") {

  object Params {
    val All = Parameter(
      nameOpt = Some("all"),
      summaryOpt = Some("Automatically stage any modified, untracked or missing files (default false)"),
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Amend = Parameter(
      nameOpt = Some("amend"),
      summaryOpt = Some("Replace the tip of the current branch by creating a new commit (default false)"),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Message = Parameter(
      nameOpt = Some("message"),
      summaryOpt = Some("Message"))
  }

  import Params._

  val params = ParameterModel(All, Amend, Message)

  def call(boundParams: BoundParams): MashObject = {
    val all = boundParams(All).isTruthy
    val amend = boundParams(Amend).isTruthy
    val message = boundParams.validateString(Message).s
    doCommit(all, amend, Some(message))
  }

  def doCommit(all: Boolean, amend: Boolean, messageOpt: Option[String]): MashObject = {
    if (all)
      StageFunction.doStage(all = true)
    GitHelper.withGit { git ⇒
      val message = messageOpt getOrElse getLastCommitMessage(git)
      val newCommit = git.commit.setMessage(message).setAmend(amend).call()
      asCommitObject(newCommit)
    }
  }

  private def getLastCommitMessage(git: Git): String = {
    val commitHistory = git.log.call().asScala.toSeq
    commitHistory.headOption.map(_.getFullMessage).getOrElse(
      throw new EvaluatorException("No previous commit to amend"))
  }

  override def typeInferenceStrategy = CommitClass

  override def summaryOpt = Some("Record changes to the repository")

}