package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.evaluator.MashNumber
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.utils.Utils

object MaxFunction extends MashFunction("collections.max") {

  object Params {
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to find the maximum of",
      isLast = true)
  }

  import Params._

  val params = ParameterModel(Seq(Sequence))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    sequence.max(Utils.AnyOrdering)
  }

  override def typeInferenceStrategy = FirstTypeInferenceStrategy

  override def summary = "Find the largest element of a sequence"

  override def descriptionOpt = Some("""Examples:
  max [1, 3, 2] # 3""")

}