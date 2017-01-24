package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.MashBoolean

object IsEmptyFunction extends MashFunction("collections.isEmpty") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence of elements to test"))
  }

  import Params._

  val params = ParameterModel(Seq(Sequence))

  def apply(arguments: Arguments): MashBoolean = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    MashBoolean(sequence.isEmpty)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(BooleanClass)

  override def summaryOpt = Some("Check whether a given sequence is empty")

  override def descriptionOpt = Some("""Examples
  isEmpty []        # true
  isEmpty [1, 2, 3] # false""")

}

