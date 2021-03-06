package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.os.OpenFunction
import com.github.mdr.mash.runtime._

object OpenMethod extends MashMethod("open") {

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashUnit =
    OpenFunction.openWithSystemOpener(target)

  override def typeInferenceStrategy = Unit

  override def summaryOpt = Some("Open this path with the default application")

}