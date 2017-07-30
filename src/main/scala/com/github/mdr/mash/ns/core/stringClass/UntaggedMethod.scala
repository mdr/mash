package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object UntaggedMethod extends MashMethod("untagged") {

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    target.asInstanceOf[MashString].copy(tagClassOpt = None)
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("This string without any tag class")
}
