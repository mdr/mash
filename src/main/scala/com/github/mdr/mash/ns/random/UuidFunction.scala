package com.github.mdr.mash.ns.random

import java.util.UUID

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashString

object UuidFunction extends MashFunction("random.uuid") {

  val params = ParameterModel()

  def call(boundParams: BoundParams): MashString =
    MashString(UUID.randomUUID.toString)

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("A random UUID")

}
