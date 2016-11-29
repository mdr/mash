package com.github.mdr.mash.ns.mash

import com.github.mdr.mash.build.BuildInfo
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime._

object VersionFunction extends MashFunction("mash.version") {

  val params = ParameterModel()

  def apply(arguments: Arguments): MashValue = {
    params.validate(arguments)
    BuildInfo.commit.map(MashString(_)).getOrElse(MashNull)
  }

  override def typeInferenceStrategy = StringClass

  override def summary = "Return the version of Mash"

}
