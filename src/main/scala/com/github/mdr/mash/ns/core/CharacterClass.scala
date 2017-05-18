package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.runtime._

object CharacterClass extends MashClass("core.Character") {

  override val methods = Seq(
    IsDigitMethod)

  object IsDigitMethod extends MashMethod("isDigit") {

    val params = ParameterModel()

    def call(target: MashValue, boundParams: BoundParams): MashBoolean = {
      val regex = target.asInstanceOf[MashString].s
      MashBoolean(regex.forall(_.isDigit))
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("Return true if this character is a digit")
  }

  override def summaryOpt: Option[String] = Some("Tag class for a character")

  override def parentOpt = Some(AnyClass)

}
