package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.SameStringMethodTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object ToLowerMethod extends MashMethod("toLower") {

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    target.asInstanceOf[MashString].modify(_.toLowerCase)
  }

  override def summaryOpt = Some("Convert string to lowercase")

  override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

}
