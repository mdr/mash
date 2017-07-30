package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.SameStringMethodTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object TrimMethod extends MashMethod("trim") {

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    target.asInstanceOf[MashString].modify(_.trim)
  }

  override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

  override def summaryOpt = Some("Strip initial and trailing whitespace")

}
