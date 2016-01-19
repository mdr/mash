package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.evaluator.MashString

object WhereNotFunction extends MashFunction("collections.whereNot") {

  val params = WhereFunction.params

  import WhereFunction.Params._

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    val input = boundParams(Sequence)
    val sequence = FunctionHelpers.interpretAsSequence(input)
    val predicate = FunctionHelpers.interpretAsFunction(boundParams(Predicate))
    val filtered = sequence.filterNot(x ⇒ Truthiness.isTruthy(predicate(x)))
    input match {
      case MashString(_, tagOpt) ⇒ filtered.asInstanceOf[Seq[MashString]].fold(MashString("", tagOpt))(_ + _)
      case _                     ⇒ filtered
    }
  }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summary = "Find all the elements in the sequence for which a predicate does not hold"

  override def descriptionOpt = Some("""Examples:
  whereNot (_ > 1) [1, 2, 3, 2, 1] # [1, 2, 2, 1]""")

}