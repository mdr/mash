package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.FunctionHelpers.asPathString
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.runtime.MashString

object HomeFunction extends MashFunction("os.home") {

  private val environmentInteractions = LinuxEnvironmentInteractions

  val params = ParameterModel()

  def apply(arguments: Arguments): MashString = {
    params.validate(arguments)
    asPathString(environmentInteractions.home)
  }

  override def typeInferenceStrategy = StringClass taggedWith PathClass

  override def summaryOpt = Some("Return the current user's home directory")

}