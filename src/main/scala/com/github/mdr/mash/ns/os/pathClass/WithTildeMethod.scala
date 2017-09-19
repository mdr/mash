package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object WithTildeMethod extends MashMethod("withTilde") {

  private val envInteractions = LinuxEnvironmentInteractions

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    val path = interpretAsPath(target)
    val retilded = new TildeExpander(envInteractions).retilde(path.toString)
    MashString(retilded)
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("Replace a home directory prefix with ~ if possible")

}
