package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.{ CharacterClass, StringClass }
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object ToCharacterMethod extends MashMethod("toCharacter") {

  override def aliases = Seq("c")

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    val s = target.asInstanceOf[MashString]
    if (s.length == 1)
      s.withTag(CharacterClass)
    else
      throw EvaluatorException("Must be a single character")
  }

  override def typeInferenceStrategy = StringClass taggedWith CharacterClass

  override def summaryOpt = Some("Tag this String, which must be a a single character, as a Character")

}

