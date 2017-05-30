package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.{ MashNull, MashValue, MashValueOrdering }

object MinFunction extends MashFunction("collections.min") {

  object Params {
    val Items = Parameter(
      nameOpt = Some("items"),
      summaryOpt = Some("Items to find the minimum of"),
      descriptionOpt = Some("""If a single argument is provided, it must be a sequence; the smallest element of the sequence is returned.
If multiple arguments are provided, the smallest argument is returned."""),
      isVariadic = true,
      variadicAtLeastOne = true,
      variadicFlatten = true)
    val Default = Parameter(
      nameOpt = Some("default"),
      summaryOpt = Some("Default value to return, if the items are empty"),
      defaultValueGeneratorOpt = Some(MashNull),
      isFlag = true,
      isFlagValueMandatory = true)
  }
  import Params._

  val params = ParameterModel(Seq(Items, Default))

  def call(boundParams: BoundParams): MashValue = {
    val default = boundParams(Default)
    val sequence = boundParams.validateSequence(Items)
    if (sequence.isEmpty)
      default
    else
      sequence.filterNot(_ == MashNull).min(MashValueOrdering)
  }

  override def typeInferenceStrategy = MaxTypeInferenceStrategy

  override def summaryOpt = Some("Find the smallest element of a sequence")

  override def descriptionOpt = Some("""Examples:
  min [1, 2, 3]      # 1
  min 1 2 3          # 1
  min [] --default=0 # 0""")

}