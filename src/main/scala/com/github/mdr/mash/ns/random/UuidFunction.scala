package com.github.mdr.mash.ns.random

import java.util.UUID

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashString

object UuidFunction extends MashFunction("random.uuid") {

  val params = ParameterModel()

  def apply(arguments: Arguments): MashString = {
    params.validate(arguments)
    MashString(UUID.randomUUID.toString)
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("A random UUID")

}
