package com.github.mdr.mash.ns.os

import com.github.mdr.mash.functions.FunctionHelpers.asPathString
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.runtime.MashString

object HomeFunction extends MashFunction("os.home") {

  private val environmentInteractions = LinuxEnvironmentInteractions

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashString = {
    asPathString(environmentInteractions.home)
  }

  override def typeInferenceStrategy = StringClass taggedWith PathClass

  override def summaryOpt = Some("Return the current user's home directory")

}