package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.{ Arguments, BinaryOperatorEvaluator }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.{ NumberClass, StringClass }
import com.github.mdr.mash.runtime.{ MashNumber, MashValue }

object SumFunction extends MashFunction("collections.sum") {

  object Params {
    val EmptyValue = Parameter(
      nameOpt = Some("emptyValue"),
      summaryOpt = Some("Value used as the sum of an empty list (default 0)"),
      defaultValueGeneratorOpt = Some(() ⇒ MashNumber(0)))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence of items to sum"),
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(EmptyValue, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    if (sequence.isEmpty)
      boundParams(EmptyValue)
    else
      sequence.reduce(BinaryOperatorEvaluator.add(_, _, None))
  }

  override def typeInferenceStrategy = SumTypeInferenceStrategy

  override def summaryOpt = Some("Sum all the elements of a sequence")

  override def descriptionOpt = Some("""Add all the elements in the sequence together, as if they were combined with the '+' operator.

Examples:
  sum [1, 2, 3]      # 6
  sum ["foo", "bar"] # "foobar"
  sum [[1, 2], [3]]  # [1, 2, 3]
  sum []             # 0
  sum "" []          # ""  
""")

}

object SumTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = SumFunction.params.bindTypes(arguments)
    val elementTypeOpt = argBindings.getType(SumFunction.Params.Sequence).collect {
      case Type.Seq(elementType) ⇒ elementType
    }
    elementTypeOpt match {
      case Some(Type.Tagged(NumberClass | StringClass, _)) ⇒ elementTypeOpt
      case Some(Type.Instance(NumberClass | StringClass)) ⇒ elementTypeOpt
      case Some(Type.Seq(_)) ⇒ elementTypeOpt
      case _ ⇒ Some(Type.Instance(NumberClass))
    }
  }

}
