package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.NoArgFunction.NoArgValue
import com.github.mdr.mash.runtime._

object ReduceFunction extends MashFunction("collections.reduce") {

  private type AccumulatorFunction = (MashValue, MashValue) ⇒ MashValue

  object Params {
    val Accumulator = Parameter(
      nameOpt = Some("accumulator"),
      summaryOpt = Some("Function to accumulate values. Must have two arguments, first the "))
    val Initial = Parameter(
      nameOpt = Some("initial"),
      summaryOpt = Some("Initial value"),
      defaultValueGeneratorOpt = Some(NoArgValue))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence of items to reduce"))
  }
  import Params._

  val params = ParameterModel(Accumulator, Initial, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val (initial, sequence) = getInitialAndSequence(boundParams)
    val accumulator = getAccumulator(boundParams)
    sequence.fold(initial)(accumulator)
  }

  private def getInitialAndSequence(boundParams: BoundParams): (MashValue, Seq[MashValue]) = {
    val sequence = boundParams.validateSequence(Sequence)
    boundParams(Initial) match {
      case NoArgValue ⇒
        val initial = sequence.headOption.getOrElse(
          boundParams.throwInvalidArgument(Sequence, "Empty sequence and no initial value provided"))
        (initial, sequence.tail)
      case initial    ⇒
        (initial, sequence)
    }
  }

  private def getAccumulator(boundParams: BoundParams): AccumulatorFunction =
    boundParams(Accumulator) match {
      case f: MashFunction ⇒
        def accumulate(acc: MashValue, item: MashValue) = {
          val accArg = EvaluatedArgument.PositionArg(SuspendedMashValue(() ⇒ acc))
          val itemArg = EvaluatedArgument.PositionArg(SuspendedMashValue(() ⇒ item))
          val args = Arguments(Seq(accArg, itemArg))
          InvocationEvaluator.callAsFunction(f, args)
        }
        accumulate
      case x ⇒
        boundParams.throwInvalidArgument(Accumulator, "Invalid accumulator function of type " + x.typeName)
    }

  override def typeInferenceStrategy = ReduceTypeInferenceStrategy

  override def summaryOpt = Some("Reduce all the elements of the sequence to a single value using a combining function")

  override def descriptionOpt = Some("""Examples:
  reduce (x y => x + y) [1, 2, 3] # 6
  reduce (x y => x + y) 1 [2, 3]  # 6
""")

}

object ReduceTypeInferenceStrategy extends TypeInferenceStrategy {

  import ReduceFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = ReduceFunction.params.bindTypes(arguments)
    val elementTypeOpt =
      for {
        ValueInfo(_, typeOpt) ← argBindings.getArgument(Sequence)
        Type.Seq(elementType) ← typeOpt
      } yield elementType

    val accumulationTypeOpt = argBindings.getType(Initial) orElse elementTypeOpt
    val functionTypeOpt = argBindings.getType(Accumulator)
    val outputTypeOpt =
      for {
        functionType ← functionTypeOpt
        itemType ← accumulationTypeOpt
        elementType ← elementTypeOpt
        outputType ← inferencer.applyFunction2(functionType, itemType, elementType)
      } yield outputType
    outputTypeOpt orElse accumulationTypeOpt
  }

}
