package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashList

object SlidingFunction extends MashFunction("collections.sliding") {

  object Params {
    val Size = Parameter(
      nameOpt = Some("size"),
      summaryOpt = Some("Size of window"))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to slide windows along"))
  }
  import Params._

  val params = ParameterModel(Seq(Size, Sequence))

  def apply(boundParams: BoundParams): MashList = {
    val n = boundParams.validateInteger(Size)
    if (n <= 0)
      boundParams.throwInvalidArgument(Size, "size must be positive")
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    MashList(sequence.sliding(n).toList.map(xs ⇒ WhereFunction.reassembleSequence(inSequence, xs)))
  }

  override def typeInferenceStrategy = SlidingTypeInferenceStrategy

  override def summaryOpt = Some("Slide a window across a sequence")

  override def descriptionOpt = Some("""Examples:
  sliding 2 [1, 2, 3] # [[1, 2], [2, 3]]""")

}

object SlidingTypeInferenceStrategy extends TypeInferenceStrategy {

  import SlidingFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
    SlidingFunction.params.bindTypes(arguments).getType(Sequence).map(Type.Seq)

}