package com.github.mdr.mash.ns.time

import java.time._

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashWrapped

object FromMillisSinceEpoch extends MashFunction("time.fromMillisSinceEpoch") {

  object Params {
    val Millis = Parameter(
      nameOpt = Some("millis"),
      summaryOpt = Some("Milliseconds since the epoch"))
  }
  import Params._

  val params = ParameterModel(Seq(Millis))
  
  def apply(arguments: Arguments): MashWrapped = {
    val boundParams = params.validate(arguments)
    val date = boundParams.validateNumber(Millis)
    MashWrapped(Instant.ofEpochMilli(date.toLong))
 }

  override def typeInferenceStrategy = DateTimeClass

  override def summaryOpt = Some("Get a DateTime representing the number of millis since the epoch")

}
