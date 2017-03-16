package com.github.mdr.mash.ns.maths

import com.github.mdr.mash.functions.{ FullyQualifiedName, _ }
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.runtime.MashNumber

object SquareRootFunction extends MashFunction("maths.squareRoot") {

  override def aliases = Seq(FullyQualifiedName("maths.sqrt"))

  object Params {
    val N = Parameter(
      nameOpt = Some("n"),
      summaryOpt = Some("Number to take the square root of"))
  }
  import Params._

  val params = ParameterModel(Seq(N))

  def apply(boundParams: BoundParams): MashNumber = {
    val n = boundParams.validateNumber(N)
    MashNumber(math.sqrt(n))
  }

  override def typeInferenceStrategy = NumberClass

  override def summaryOpt = Some("Calculate the square root of a given value")
}