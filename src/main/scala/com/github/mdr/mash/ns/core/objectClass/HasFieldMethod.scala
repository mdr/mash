package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashObject, MashValue }

object HasFieldMethod extends MashMethod("hasField") {

  object Params {
    val Name = Parameter(
      nameOpt = Some("name"),
      summaryOpt = Some("Field name"))
  }

  import Params._

  val params = ParameterModel(Name)

  def call(target: MashValue, boundParams: BoundParams): MashBoolean = {
    val obj = target.asInstanceOf[MashObject]
    val field = boundParams(Name)
    MashBoolean(obj hasField field)
  }

  override def typeInferenceStrategy = Boolean

  override def summaryOpt = Some("Return true if this object contains the given field")

  override val isShy = true

}
