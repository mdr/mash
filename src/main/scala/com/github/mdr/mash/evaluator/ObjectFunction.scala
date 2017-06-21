package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.{ MashNull, MashObject, MashValue }

class ObjectFunction(obj: MashObject,
                     functionLocationOpt: Option[SourceLocation],
                     invocationLocationOpt: Option[SourceLocation]) extends MashFunction() {

  object Params {
    val Field = Parameter(
      nameOpt = Some("field"),
      summaryOpt = Some("Field to look up in the object"))
  }

  import Params._

  val params = ParameterModel(Field)

  def call(boundParams: BoundParams): MashValue = {
    val field = boundParams.validateString(Field).s
    obj.get(field).getOrElse(MashNull)
  }

  override def summaryOpt = Some("Object as a function")

}
