package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashList, MashValue }

object ReverseFunction extends MashFunction("collections.reverse") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to reverse"))
  }

  import Params._

  val params = ParameterModel(Sequence)

  def call(boundParams: BoundParams): MashValue =
    SequenceLikeAnalyser.analyse(boundParams, Sequence) {
      case SequenceLike.List(items) ⇒ MashList(items.reverse)
      case SequenceLike.String(s)   ⇒ s.reverse
      case SequenceLike.Object(obj) ⇒ obj.reverse
    }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy(params, Sequence)

  override def summaryOpt = Some("Reverse a List, String or Object")

  override def descriptionOpt = Some(
    """Examples:
  reverse [1, 2, 3] # [3, 2, 1]
  reverse []        # []
  reverse "part"    # "trap""")

}