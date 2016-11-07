package com.github.mdr.mash.ns.time
import java.time._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.runtime.MashWrapped
import com.github.mdr.mash.functions.Parameter
import java.time.format.DateTimeFormatter

object ParseIso8601Function extends MashFunction("time.parseIso8601") {

  object Params {
    val Date = Parameter(
      name = "date",
      summary = "Date in ISO-8601 format")
  }
  import Params._

  val params = ParameterModel(Seq(Date))
  
  def apply(arguments: Arguments): MashWrapped = {
    val boundParams = params.validate(arguments)
    val date = boundParams.validateString(Date).s
    MashWrapped(Instant.from(DateTimeFormatter.ISO_DATE_TIME.parse(date)))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(DateTimeClass)

  override def summary = "Parse the given string as a date/time in ISO-8601 format"

}
