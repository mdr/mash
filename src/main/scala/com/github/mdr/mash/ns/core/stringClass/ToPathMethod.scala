package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object ToPathMethod extends MashMethod("toPath") {

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    target.asInstanceOf[MashString].withTag(PathClass)
  }

  override def summaryOpt = Some("Tag this string as a path")

  override def typeInferenceStrategy = StringClass taggedWith PathClass

}
