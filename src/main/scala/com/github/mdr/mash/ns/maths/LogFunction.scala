package com.github.mdr.mash.ns.maths

import scala.collection.immutable.ListMap
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.ns.core.NumberClass

object LogFunction extends MashFunction("maths.log") {

  object Params {
    val N = Parameter(
      name = "n",
      summary = "Number to take the logarithm of")
    val Base = Parameter(
      name = "base",
      summary = "base",
      defaultValueGeneratorOpt = Some(() ⇒ MashNumber(Math.E)))
  }
  import Params._

  val params = ParameterModel(Seq(N, Base))

  def apply(arguments: Arguments): MashNumber = {
    val boundParams = params.validate(arguments)
    val n = boundParams.validateNumber(N)
    val base = boundParams.validateNumber(Base)
    MashNumber(Math.log(n) / Math.log(base))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(NumberClass)

  override def summary = "Calculate the logarithm of a given value"
}