package com.github.mdr.mash.ns.git.stash


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
import com.github.mdr.mash.ns.git.GitHelper

object StashCreateFunction extends MashFunction("git.stash.create") {

  private val filesystem = LinuxFileSystem

  object Params {
  }
  
  import Params._

  val params = ParameterModel()

  def apply(arguments: Arguments) {
    params.validate(arguments)
    GitHelper.withGit { git ⇒
      git.stashCreate().call()
    }
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def summary = "Stash changes in the working directory"

}