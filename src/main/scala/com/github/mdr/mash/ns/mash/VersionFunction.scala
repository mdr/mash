package com.github.mdr.mash.ns.mash

import com.github.mdr.mash.build.BuildInfo
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime._

object VersionFunction extends MashFunction("mash.version") {

  val params = ParameterModel()

  def call(boundParams: BoundParams): MashValue = {
    MashString(BuildInfo.version)
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("Return the version of Mash")

}
