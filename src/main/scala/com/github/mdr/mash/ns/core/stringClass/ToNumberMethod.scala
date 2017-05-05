package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.runtime.{ MashNumber, MashString, MashValue }

object ToNumberMethod extends MashMethod("toNumber") {

  val params = ParameterModel()

  def call(target: MashValue, boundParams: BoundParams): MashNumber = {
    MashNumber(target.asInstanceOf[MashString].s.toDouble)
  }

  override def summaryOpt = Some("Parse this string as a number")

  override def typeInferenceStrategy = NumberClass

}
