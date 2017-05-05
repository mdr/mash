package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.runtime.{ MashNumber, MashString, MashValue }

object LengthMethod extends MashMethod("length") {

  val params = ParameterModel()

  def call(target: MashValue, boundParams: BoundParams): MashNumber = {
    target.asInstanceOf[MashString].length
  }

  override def typeInferenceStrategy = NumberClass

  override def summaryOpt = Some("Length of this string")

}
