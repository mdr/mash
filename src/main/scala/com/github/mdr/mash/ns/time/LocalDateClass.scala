package com.github.mdr.mash.ns.time

import java.time._
import java.time.temporal.ChronoUnit
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel

object LocalDateClass extends MashClass("time.Date") {

  override def summary = "A date"

}