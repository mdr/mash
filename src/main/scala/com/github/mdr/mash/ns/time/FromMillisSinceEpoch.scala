package com.github.mdr.mash.ns.time

import java.time._

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashWrapped

object FromMillisSinceEpoch extends MashFunction("time.fromMillisSinceEpoch") {

  object Params {
    val Millis = Parameter(
      nameOpt = Some("millis"),
      summaryOpt = Some("Milliseconds since the epoch"))
  }
  import Params._

  val params = ParameterModel(Seq(Millis))
  
  def call(boundParams: BoundParams): MashWrapped = {
    val date = boundParams.validateNumber(Millis)
    MashWrapped(Instant.ofEpochMilli(date.toLong))
 }

  override def typeInferenceStrategy = DateTimeClass

  override def summaryOpt = Some("Get a DateTime representing the number of millis since the epoch")

}
