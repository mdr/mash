package com.github.mdr.mash.ns.git.stash

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.MashUnit

object CreateFunction extends MashFunction("git.stash.create") {

  private val filesystem = LinuxFileSystem

  val params = ParameterModel()

  def apply(arguments: Arguments): MashUnit = {
    params.validate(arguments)
    GitHelper.withGit { git ⇒
      git.stashCreate.call()
    }
    MashUnit
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Stash changes in the working directory"

}