package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.runtime.{ MashList, MashString, MashValue }
import com.github.mdr.mash.utils.StringUtils

object LinesMethod extends MashMethod("lines") {

  val params = ParameterModel()

  def apply(target: MashValue, boundParams: BoundParams): MashList = {
    val targetString = target.asInstanceOf[MashString]
    val pieces = StringUtils.splitIntoLines(targetString.s)
    MashList(pieces.map(MashString(_, targetString.tagClassOpt)))
  }

  override def typeInferenceStrategy = SplitMethod.typeInferenceStrategy

  override def summaryOpt = Some("Split this string into a sequence of lines")

}
