package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.SameStringMethodTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object ToUpperMethod extends MashMethod("toUpper") {

  val params = ParameterModel()

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    target.asInstanceOf[MashString].modify(_.toUpperCase)
  }

  override def summaryOpt = Some("Convert string to uppercase")

  override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

}
