package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.MashUnit
import org.eclipse.jgit.api.Git

object InitFunction extends MashFunction("git.init") {

  val params = ParameterModel(Seq())

  def apply(arguments: Arguments): MashUnit = {
    params.validate(arguments)
    Git.init.call()
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Create an empty Git repository")

}