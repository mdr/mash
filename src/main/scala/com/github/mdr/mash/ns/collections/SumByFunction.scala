package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.BinaryOperatorEvaluator
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashNumber, MashValue }

object SumByFunction extends MashFunction("collections.sumBy") {

  object Params {
    val Attribute = Parameter(
      nameOpt = Some("attribute"),
      summaryOpt = Some("Function to extract a value from to sum"))
    val EmptyValue = Parameter(
      nameOpt = Some("emptyValue"),
      summaryOpt = Some("Value used as the sum of an empty list (default 0)"),
      defaultValueGeneratorOpt = Some(MashNumber(0)))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to find the sum of"),
      isLast = true)
  }

  import Params._

  val params = ParameterModel(Seq(Attribute, EmptyValue, Sequence))

  def apply(boundParams: BoundParams): MashValue = {
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

  override def summaryOpt = Some("Sum the elements of a sequence using an attribute")

  override def descriptionOpt = Some(
    """Examples:
  sumBy length ["foo", "bar", "baz"] # 9""")

}

object SumByTypeInferenceStrategy extends TypeInferenceStrategy {

  import SumByFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = SumByFunction.params.bindTypes(arguments)
    val sequenceTypeOpt = argBindings.getType(Sequence)
    val attributeArg = argBindings.getArgument(Attribute)
    val mappedTypeOpt = MapTypeInferenceStrategy.inferMappedType(inferencer, attributeArg, sequenceTypeOpt)
    TypeInferencer.inferTypeAdd(mappedTypeOpt, mappedTypeOpt)
  }

}