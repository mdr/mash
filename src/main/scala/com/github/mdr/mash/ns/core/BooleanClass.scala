package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.runtime.{ MashBoolean, MashValue }

object BooleanClass extends MashClass("core.Boolean") {

  override val methods = Seq(
    NotMethod)

  object NotMethod extends MashMethod("not") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashBoolean = {
      target.asInstanceOf[MashBoolean].negate
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("Negate this boolean")

    override def descriptionOpt = Some("""Examples:
<mash>
    true.negate  # false
    false.negate # true
</mash>""")

  }

  override def parentOpt = Some(AnyClass)
  
  override def summaryOpt = Some("Boolean values true and false")
}