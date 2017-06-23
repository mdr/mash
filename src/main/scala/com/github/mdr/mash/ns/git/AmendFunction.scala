package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.NoArgFunction.NoArgValue
import com.github.mdr.mash.ns.git.LogFunction.asCommitObject
import com.github.mdr.mash.runtime.MashObject
import org.eclipse.jgit.api.Git

import scala.collection.JavaConverters._

object AmendFunction extends MashFunction("git.amend") {

  import CommitFunction.Params.All

  object Params {
    val Message = Parameter(
      nameOpt = Some("message"),
      summaryOpt = Some("Message (defaults to last commit message)"),
      defaultValueGeneratorOpt = Some(() ⇒ NoArgValue))
  }

  import Params._

  val params = ParameterModel(Message, All)

  def call(boundParams: BoundParams): MashObject = {
    val all = boundParams(All).isTruthy
    val messageOpt = boundParams.validateStringOpt(Message).map(_.s)
    GitHelper.withGit { git ⇒
      val message = messageOpt getOrElse getLastCommitMessage(git)
      val newCommit = git.commit.setMessage(message).setAll(all).setAmend(true).call()
      asCommitObject(newCommit)
    }
  }

  private def getLastCommitMessage(git: Git): String = {
    val commitHistory = git.log.call().asScala.toSeq
    commitHistory.headOption.map(_.getFullMessage).getOrElse(
      throw new EvaluatorException("No previous commit to amend"))
  }

  override def typeInferenceStrategy = CommitClass

  override def summaryOpt = Some("Amend the previous commit")

}