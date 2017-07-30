package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.{ RegexClass, StringClass }
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object RegexMethod extends MashMethod("regex") {

  override def aliases = Seq("r")

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    target.asInstanceOf[MashString].withTag(RegexClass)
  }

  override def typeInferenceStrategy = StringClass taggedWith RegexClass

  override def summaryOpt = Some("This string as a regular expression")

}
