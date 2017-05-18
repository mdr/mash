package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.{ CharacterClass, StringClass }
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashValue }

object CharMethod extends MashMethod("char") {

  override def aliases = Seq("c")

  val params = ParameterModel()

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    val s = target.asInstanceOf[MashString]
    if (s.length == 1)
      s.withTag(CharacterClass)
    else
      throw new EvaluatorException("Must be a single character")
  }

  override def typeInferenceStrategy = StringClass taggedWith CharacterClass

  override def summaryOpt = Some("Confirm and tag this String as a character")

}

