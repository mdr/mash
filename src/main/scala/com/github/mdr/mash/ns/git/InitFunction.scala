package com.github.mdr.mash.ns.git

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.MashUnit
import org.eclipse.jgit.api.Git

object InitFunction extends MashFunction("git.init") {

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashUnit = {
    Git.init.call()
    MashUnit
  }

  override def typeInferenceStrategy = Unit

  override def summaryOpt = Some("Create an empty Git repository")

}