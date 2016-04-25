package com.github.mdr.mash.ns.git.stash

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.os.linux.LinuxFileSystem

object StashApplyFunction extends MashFunction("git.stash.apply") {

  private val filesystem = LinuxFileSystem

  object Params {
  }

  import Params._

  val params = ParameterModel()

  def apply(arguments: Arguments) {
    params.validate(arguments)
    GitHelper.withGit { git ⇒
      git.stashApply().call()
    }
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def summary = "Apply stashed changes to the current working directory"

}