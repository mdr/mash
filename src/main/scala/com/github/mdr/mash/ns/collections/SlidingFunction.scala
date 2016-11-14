package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashList

object SlidingFunction extends MashFunction("collections.sliding") {

  object Params {
    val Size = Parameter(
      name = "size",
      summary = "Size of window")
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to slide windows along")
  }
  import Params._

  val params = ParameterModel(Seq(Size, Sequence))

  def apply(arguments: Arguments): MashList = {
    val boundParams = params.validate(arguments)
    val n = boundParams.validateInteger(Size)
    if (n <= 0)
      boundParams.throwInvalidArgument(Size, "size must be positive")
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    MashList(sequence.sliding(n).toList.map(xs => WhereFunction.reassembleSequence(inSequence, xs)))
  }

  override def typeInferenceStrategy = SlidingTypeInferenceStrategy

  override def summary = "Slide a window across a sequence"

  override def descriptionOpt = Some("""Examples:
  sliding 2 [1, 2, 3] # [[1, 2], [2, 3]]""")

}

object SlidingTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = SlidingFunction.params.bindTypes(arguments)
    import SlidingFunction.Params._
    val sequenceExprOpt = argBindings.get(Sequence)
    for {
      AnnotatedExpr(_, sequenceTypeOpt) ← sequenceExprOpt
      sequenceType ← sequenceTypeOpt
    } yield Type.Seq(sequenceType)
  }

}