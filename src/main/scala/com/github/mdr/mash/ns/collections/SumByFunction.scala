package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashNumber
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.NumberClass

object SumByFunction extends MashFunction("collections.sumBy") {

  object Params {
    val Attribute = Parameter(
      name = "attribute",
      summary = "Function to extract a numeric value to sum")
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to find the sum of",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Attribute, Sequence))

  def apply(arguments: Arguments): MashNumber = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    val attribute = boundParams.validateFunction(Attribute)
    val numbers = sequence.map(attribute).asInstanceOf[Seq[MashNumber]]
    numbers.foldRight(MashNumber(0))(_ + _)
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