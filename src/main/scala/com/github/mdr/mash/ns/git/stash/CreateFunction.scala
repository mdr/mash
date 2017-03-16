package com.github.mdr.mash.ns.git.stash

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.MashUnit

object CreateFunction extends MashFunction("git.stash.create") {

  private val filesystem = LinuxFileSystem

  val params = ParameterModel()

  def apply(boundParams: BoundParams): MashUnit = {
    GitHelper.withGit { git ⇒
      git.stashCreate.call()
    }
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Stash changes in the working directory")

}