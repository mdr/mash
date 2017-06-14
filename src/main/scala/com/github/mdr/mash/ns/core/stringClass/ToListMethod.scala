package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.CharacterClass
import com.github.mdr.mash.runtime._

object ToListMethod extends MashMethod("toList") {

  val params = ParameterModel()

  def call(target: MashValue, boundParams: BoundParams): MashList =
    MashList(target.asInstanceOf[MashString].characterSequence)

  override def summaryOpt = Some("A List of this String's characters")

  override def typeInferenceStrategy = Seq(CharacterClass)

}
