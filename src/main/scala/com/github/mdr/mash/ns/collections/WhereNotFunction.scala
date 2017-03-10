package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.objectClass.WhereNotMethod
import com.github.mdr.mash.runtime._

object WhereNotFunction extends MashFunction("collections.whereNot") {

  val params = WhereFunction.params

  import WhereFunction.Params._

  def apply(boundParams: BoundParams): MashValue =
    boundParams(Sequence) match {
      case obj: MashObject ⇒ WhereNotMethod.doWhereNot(obj, boundParams)
      case inSequence      ⇒
        val sequence = boundParams.validateSequence(Sequence)
        val predicate = boundParams.validateFunction(Predicate)
        val newSequence = sequence.filterNot(x ⇒ predicate(x).isTruthy)
        WhereFunction.reassembleSequence(inSequence, newSequence)
    }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summaryOpt = Some("Find all the elements in the sequence for which a predicate does not hold")

  override def descriptionOpt = Some(
    """Examples:
  whereNot (_ > 1) [1, 2, 3, 2, 1] # [1, 2, 2, 1]""")

}