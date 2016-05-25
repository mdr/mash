package com.github.mdr.mash.ns.git

import org.eclipse.jgit.api.Git
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.ns.core.UnitClass

object CommitFunction extends MashFunction("git.commit") {

  private val filesystem = LinuxFileSystem

  object Params {
    val Message = Parameter(
      name = "message",
      summary = "Message")
    val All = Parameter(
      name = "all",
      summary = "Automatically stage files that have been modified and deleted, but new files are not affected (default false)",
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isBooleanFlag = true)
    val Amend = Parameter(
      name = "amend",
      summary = "Replace the tip of the current branch by creating a new commit (default false)",
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Message, All, Amend))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val all = Truthiness.isTruthy(boundParams(All))
    val amend = Truthiness.isTruthy(boundParams(Amend))
    val message = boundParams.validateString(Message).s
    GitHelper.withGit { git =>
      git.commit.setMessage(message).setAll(all).setAmend(amend).call()
    }
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def summary = "Record changes to the repository"

}