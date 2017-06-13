package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.objectClass.{ WhereMethod, WhereNotMethod }
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.ns.collections.ToListHelper.tryToList

object WhereNotFunction extends MashFunction("collections.whereNot") {

  val params = WhereFunction.params

  import WhereFunction.Params._

  def call(boundParams: BoundParams): MashValue = {
    val predicate = boundParams.validateFunction(Predicate)
    SequenceLikeAnalyser.analyse(boundParams, Sequence) {
      case SequenceLike.Items(items)   ⇒ MashList(items.filterNot(x ⇒ predicate(x).isTruthy))
      case string: SequenceLike.String ⇒ string.reassemble(string.characterSequence.filterNot(x ⇒ predicate(x).isTruthy))
      case SequenceLike.Object(obj)    ⇒ WhereNotMethod.doWhereNot(obj, boundParams)
    }
  }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summaryOpt = Some("Find all the elements in the sequence for which a predicate does not hold")

  override def descriptionOpt = Some(
    """Examples:
  whereNot (_ > 1) [1, 2, 3, 2, 1] # [1, 2, 2, 1]""")

}