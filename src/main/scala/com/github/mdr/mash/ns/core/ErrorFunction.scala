package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashValue

object ErrorFunction extends MashFunction("core.error") {

  object Params {
    val Message = Parameter(
      nameOpt = Some("message"),
      summaryOpt = Some("Error message to produce"))
  }

  import Params._

  val params = ParameterModel(Seq(Message))

  def call(boundParams: BoundParams): MashValue = {
    val message = boundParams.validateString(Message)
    throw new EvaluatorException(message.s)
  }

  override def summaryOpt = Some("Throw an exception with the given message")

}