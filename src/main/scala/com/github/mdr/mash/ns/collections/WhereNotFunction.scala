package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.objectClass.WhereNotMethod
import com.github.mdr.mash.runtime._

object WhereNotFunction extends MashFunction("collections.whereNot") {

  val params = WhereFunction.params

  import WhereFunction.Params._

  def call(boundParams: BoundParams): MashValue =
    SequenceLikeAnalyser.analyse(boundParams, Sequence) {
      case list: SequenceLike.List     ⇒ list.reassemble(doWhereNot(list.items, boundParams))
      case string: SequenceLike.String ⇒ string.reassemble(doWhereNot(string.items, boundParams))
      case SequenceLike.Object(obj)    ⇒ WhereNotMethod.doWhereNot(obj, boundParams)
    }

  private def doWhereNot(items: Seq[MashValue], boundParams: BoundParams): Seq[MashValue] = {
    val predicate = boundParams.validateFunction(Predicate)
    items.filterNot(x ⇒ predicate(x).isTruthy)
  }

  override def typeInferenceStrategy = WhereTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summaryOpt = Some("Find all the elements in the sequence for which a predicate does not hold")

  override def descriptionOpt = Some(
    """Examples:
<mash>
  whereNot (_ > 1) [1, 2, 3, 2, 1] # [1, 2, 2, 1]
</mash>""")

}