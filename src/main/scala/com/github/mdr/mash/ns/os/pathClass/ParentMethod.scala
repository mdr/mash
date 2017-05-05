package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.{ MashNull, MashValue }

object ParentMethod extends MashMethod("parent") {

  val params = ParameterModel()

  def call(target: MashValue, boundParams: BoundParams): MashValue = {
    val parent = interpretAsPath(target).getParent
    if (parent == null)
      MashNull
    else
      asPathString(parent)
  }

  override def typeInferenceStrategy = StringClass taggedWith PathClass

  override def summaryOpt = Some("The parent of this path")
}
