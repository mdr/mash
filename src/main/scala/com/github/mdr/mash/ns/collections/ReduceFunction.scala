package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluatedArgument
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.evaluator.InvocationEvaluator
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.inference.TypeInferenceStrategy
import com.github.mdr.mash.inference.AnnotatedExpr
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.inference.Inferencer
import com.github.mdr.mash.inference.Type

object ReduceFunction extends MashFunction("collections.reduce") {

  object Params {
    val Accumulator = Parameter(
      name = "accumulator",
      summary = "Function to accumulate values")
    val Initial = Parameter(
      name = "initial",
      summary = "Initial value",
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence of items to reduce",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Accumulator, Initial, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    val initial = boundParams(Initial) match {
      case MashNull ⇒ sequence.headOption.getOrElse(
        boundParams.throwInvalidArgument(Sequence, "Empty sequence and no initial value provided"))
      case v ⇒ v
    }
    val f = boundParams(Accumulator) match {
      case f: MashFunction ⇒
        (acc: MashValue, item: MashValue) ⇒ {
          val args = Arguments(Seq(EvaluatedArgument.PositionArg(acc, None), EvaluatedArgument.PositionArg(item, None)))
          InvocationEvaluator.callFunction(f, args)
        }
        case x ⇒
        boundParams.throwInvalidArgument(Accumulator, "Invalid accumulator function of type " + x.typeName)
    }
    sequence.fold(initial)(f)
  }

  override def typeInferenceStrategy = ReduceTypeInferenceStrategy

  override def summary = "Reduce all the elements of the sequence to a single value using a combining function"

  override def descriptionOpt = Some("""Examples:
  reduce [1, 2, 3] (x y => x + y) # 6
  reduce [2 3] (x y => x * y) 1   # 6
""")

}

object ReduceTypeInferenceStrategy extends TypeInferenceStrategy {

  import ReduceFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = ReduceFunction.params.bindTypes(arguments)
    val elementTypeOpt =
      for {
        AnnotatedExpr(_, typeOpt) ← argBindings.get(Sequence)
        Type.Seq(elementType) ← typeOpt
      } yield elementType

    val accumulationTypeOpt = argBindings.get(Initial).flatMap(_.typeOpt) orElse elementTypeOpt
    val functionTypeOpt = argBindings.get(Accumulator).flatMap(_.typeOpt)
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
