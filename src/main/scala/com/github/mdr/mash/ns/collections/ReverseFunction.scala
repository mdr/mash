package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.ns.collections.ToListHelper.tryToList
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }

object ReverseFunction extends MashFunction("collections.reverse") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to reverse"))
  }

  import Params._

  val params = ParameterModel(Sequence)

  def call(boundParams: BoundParams): MashValue =
    boundParams(Sequence) match {
      case s: MashString   ⇒ s.reverse
      case xs: MashList    ⇒ MashList(xs.immutableElements.reverse)
      case obj: MashObject ⇒
        tryToList(obj) match {
          case Some(items) ⇒ MashList(items.reverse)
          case None        ⇒ obj.reverse
        }
      case value           ⇒
        boundParams.throwInvalidArgument(Sequence, s"Invalid argument '${Sequence.name}'. Must be a List, String or Object, but was a ${value.typeName}")
    }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy(params, Sequence)

  override def summaryOpt = Some("Reverse a List, String or Object")

  override def descriptionOpt = Some(
    """Examples:
  reverse [1, 2, 3] # [3, 2, 1]
  reverse []        # []
  reverse 'part'    # 'trap'""")

}