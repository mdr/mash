package com.github.mdr.mash.ns.random

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.runtime.MashNumber

object NumberFunction extends MashFunction("random.number") {

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashNumber =
    MashNumber(math.random())

  override def typeInferenceStrategy = NumberClass

  override def summaryOpt = Some("A random number between 0 (inclusive) and 1 (exclusive)")

}
