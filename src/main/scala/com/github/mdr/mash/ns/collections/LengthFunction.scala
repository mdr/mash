package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.runtime.MashNumber

object LengthFunction extends MashFunction("collections.length") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to find the length of"))
  }
  import Params._

  val params = ParameterModel(Seq(Sequence))

  def apply(boundParams: BoundParams): MashNumber = {
    val sequence = boundParams.validateSequence(Sequence)
    MashNumber(sequence.length)
  }

  override def typeInferenceStrategy = NumberClass

  override def summaryOpt = Some("Find the length of a sequence")

  override def descriptionOpt = Some("""Examples:
  length [1, 2, 3] # 3
  length []        # 0""")

}