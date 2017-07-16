package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime._

object MaxFunction extends MashFunction("collections.max") {

  object Params {
    val Items = Parameter(
      nameOpt = Some("items"),
      summaryOpt = Some("Items to find the maximum of"),
      descriptionOpt = Some(
        """If a single argument is provided, it must be a sequence; the largest element of the sequence is returned.
If multiple arguments are provided, the largest argument is returned."""),
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

  val params = ParameterModel(Items, Default)

  def call(boundParams: BoundParams): MashValue = {
    val default = boundParams(Default)
    val sequence = boundParams.validateSequence(Items)
    if (sequence.isEmpty)
      default
    else
      sequence.filterNot(_ == MashNull).max(MashValueOrdering)
  }

  override def typeInferenceStrategy = MaxTypeInferenceStrategy

  override def summaryOpt = Some("Find the largest element of a sequence")

  override def descriptionOpt = Some(
    """Examples:
  max [1, 2, 3]      # 3
  max 1 2 3          # 3
  max [] --default=0 # 0""")

}

object MaxTypeInferenceStrategy extends TypeInferenceStrategy {

  import MaxFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = MaxFunction.params.bindTypes(arguments)
    argBindings.getType(Items).collect { case Type.Seq(elementType) ⇒ elementType }
  }

}