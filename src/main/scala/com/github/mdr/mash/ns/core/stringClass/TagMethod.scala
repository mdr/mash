package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.ClassClass
import com.github.mdr.mash.runtime.{ MashNull, MashString, MashValue }

object TagMethod extends MashMethod("tag") {

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashValue = {
    target.asInstanceOf[MashString].tagClassOpt.getOrElse(MashNull)
  }

  override def typeInferenceStrategy = ClassClass

  override def summaryOpt = Some("This string's tagged type, if any")
}
