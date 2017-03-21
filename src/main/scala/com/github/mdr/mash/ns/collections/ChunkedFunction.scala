package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashList

object ChunkedFunction extends MashFunction("collections.chunked") {

  object Params {
    val Size = Parameter(
      nameOpt = Some("size"),
      summaryOpt = Some("Number of elements in each chunk"))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to split into chunks"))
  }
  import Params._

  val params = ParameterModel(Seq(Size, Sequence))

  def apply(boundParams: BoundParams): MashList = {
    val n = boundParams.validateInteger(Size)
    if (n <= 0)
      boundParams.throwInvalidArgument(Size, "size must be positive")
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    MashList(sequence.grouped(n).toList.map(xs ⇒ WhereFunction.reassembleSequence(inSequence, xs)))
  }

  override def typeInferenceStrategy = SlidingTypeInferenceStrategy

  override def summaryOpt = Some("Split a sequence up into chunks of a given size")

  override def descriptionOpt = Some("""Returns a list of chunks (sequences of elements of the given size).
Any elements left over will be placed into a final chunk.
Examples:
  chunked 2 [1, 2, 3, 4, 5] # [[1, 2], [3, 4], [5]]""")

}
