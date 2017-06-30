package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.CharacterClass
import com.github.mdr.mash.runtime._

object ToListMethod extends MashMethod("toList") {

  override def aliases = Seq("characters")

  val params = ParameterModel()

  def call(target: MashValue, boundParams: BoundParams): MashList =
    MashList(target.asInstanceOf[MashString].characterSequence)

  override def summaryOpt = Some("A List of this String's characters")

  override def typeInferenceStrategy = Seq(CharacterClass)

  override def descriptionOpt = Some(
    """Returns a list of Character-tagged Strings.

Example:
  'string'.characters  # ['s', 't', 'r', 'i', 'n', 'g']""")


}
