package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.utils.Utils

object MinFunction extends MashFunction("collections.min") {

  object Params {
    val Items = Parameter(
      name = "items",
      summary = "Items to find the minimum of",
      descriptionOpt = Some("""If a single argument is provided, it must be a sequence; the smallest element of the sequence is returned.
If multiple arguments are provided, the smallest argument is returned."""),
      isVariadic = true)
  }
  import Params._

  val params = ParameterModel(Seq(Items))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    val sequence = MaxFunction.getSequence(boundParams, Items)
    sequence.min(Utils.AnyOrdering)
  }

  override def typeInferenceStrategy = MaxTypeInferenceStrategy

  override def summary = "Find the smallest element of a sequence"

  override def descriptionOpt = Some("""Examples:
  min [1, 2, 3] # 1
  min 1 2 3     # 1""")

}