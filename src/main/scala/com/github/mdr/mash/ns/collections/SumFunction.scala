package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.inference.TypeInferenceStrategy
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.inference.Inferencer
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.evaluator.MashNumber
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.AnnotatedExpr
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.ns.core.StringClass

object SumFunction extends MashFunction("collections.sum") {

  object Params {
    val Default = Parameter(
      name = "emptyValue",
      summary = "Value used as the sum of an empty list (default 0)",
      defaultValueGeneratorOpt = Some(() ⇒ MashNumber(0)))
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence of items to sum",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Default, Sequence))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams(Sequence) match {
      case xs: Seq[_] ⇒ xs
      case _          ⇒ throw new EvaluatorException("Invalid arguments for function 'sum': Argument 'sequence' must be a sequence")
    }
    if (sequence.isEmpty)
      boundParams(Default)
    else
      sequence.reduce(Evaluator.add(_, _, None))
  }

  override def typeInferenceStrategy = SumTypeInferenceStrategy

  override def summary = "Sum all the elements of a sequence"

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
    val elementTypeOpt =
      for {
        AnnotatedExpr(_, typeOpt) ← argBindings.get(SumFunction.Params.Sequence)
        Type.Seq(elementType) ← typeOpt
      } yield elementType
    elementTypeOpt match {
      case Some(Type.Tagged(NumberClass | StringClass, _)) ⇒ elementTypeOpt
      case Some(Type.Instance(NumberClass | StringClass)) ⇒ elementTypeOpt
      case Some(Type.Seq(_)) ⇒ elementTypeOpt
      case _ ⇒ Some(Type.Instance(NumberClass))
    }
  }

}
