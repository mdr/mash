package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.evaluator.BinaryOperatorEvaluator

object SumByFunction extends MashFunction("collections.sumBy") {

  object Params {
    val Attribute = Parameter(
      name = "attribute",
      summary = "Function to extract a value from to sum")
    val EmptyValue = Parameter(
      name = "emptyValue",
      summary = "Value used as the sum of an empty list (default 0)",
      defaultValueGeneratorOpt = Some(() ⇒ MashNumber(0)))
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to find the sum of",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Attribute, EmptyValue, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    val attribute = boundParams.validateFunction(Attribute)
    val attributes = sequence.map(attribute)
    if (attributes.isEmpty)
      boundParams(EmptyValue)
    else
      attributes.reduce(BinaryOperatorEvaluator.add(_, _, None))
  }

  override def typeInferenceStrategy = SumByTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summary = "Sum the elements of a sequence using an attribute"

  override def descriptionOpt = Some("""Examples:
  sumBy length ["foo", "bar", "baz"] # 9""")

}

object SumByTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = SumByFunction.params.bindTypes(arguments)
    import SumByFunction.Params._
    val inferredTypeOpt = MapTypeInferenceStrategy.inferAppliedType(inferencer, argBindings.get(Attribute), argBindings.get(Sequence))
    inferredTypeOpt match {
      case Some(Type.Tagged(NumberClass, _)) ⇒ inferredTypeOpt
      case _                                 ⇒ Some(Type.Instance(NumberClass))
    }
  }

}