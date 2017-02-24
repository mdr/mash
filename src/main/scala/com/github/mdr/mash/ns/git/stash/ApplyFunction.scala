package com.github.mdr.mash.ns.git.stash

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.MashUnit

object ApplyFunction extends MashFunction("git.stash.apply") {

  private val filesystem = LinuxFileSystem

  val params = ParameterModel()

  def apply(boundParams: BoundParams): MashUnit = {
    GitHelper.withGit { git ⇒
      git.stashApply.call()
    }
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Apply stashed changes to the current working directory")

}