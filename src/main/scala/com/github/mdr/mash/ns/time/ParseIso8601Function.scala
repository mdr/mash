package com.github.mdr.mash.ns.time
import java.time._
import java.time.format.DateTimeFormatter

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashWrapped

object ParseIso8601Function extends MashFunction("time.parseIso8601") {

  object Params {
    val Date = Parameter(
      nameOpt = Some("date"),
      summaryOpt = Some("Date in ISO-8601 format"))
  }
  import Params._

  val params = ParameterModel(Seq(Date))
  
  def apply(boundParams: BoundParams): MashWrapped = {
    val date = boundParams.validateString(Date).s
    MashWrapped(Instant.from(DateTimeFormatter.ISO_DATE_TIME.parse(date)))
  }

  override def typeInferenceStrategy = DateTimeClass

  override def summaryOpt = Some("Parse the given string as a date/time in ISO-8601 format")

}
