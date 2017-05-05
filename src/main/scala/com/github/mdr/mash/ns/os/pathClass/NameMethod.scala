package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object NameMethod extends MashMethod("name") {

  val params = ParameterModel()

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    asPathString(interpretAsPath(target).getFileName)
  }

  override def typeInferenceStrategy = StringClass taggedWith PathClass

  override def summaryOpt = Some("Name (last segment) of this path")

}
