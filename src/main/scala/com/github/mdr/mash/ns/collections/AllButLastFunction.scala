package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.runtime._

object AllButLastFunction extends MashFunction("collections.allButLast") {

  object Params {
    lazy val N: Parameter = Parameter(
      nameOpt = Some("n"),
      summaryOpt = Some("Number of elements to omit (default 1)"),
      defaultValueGeneratorOpt = Some(MashNumber(1)))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to find the value(s) of"),
      descriptionOpt = Some("Can be a List, String or Object"))
  }

  import Params._

  val params = ParameterModel(N, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val n = boundParams.validateNonNegativeInteger(N)
    SequenceLikeAnalyser.analyse(boundParams, Sequence) {
      case SequenceLike.List(items) ⇒ MashList(items dropRight n)
      case SequenceLike.String(s)   ⇒ s.modify(_ dropRight n)
      case SequenceLike.Object(obj) ⇒ MashObject.of(obj.immutableFields dropRight n)
    }
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy(params, Sequence)

  override def summaryOpt = Some("Take all but the last element(s) of a sequence")

  override def descriptionOpt = Some(
    s"""If a count ${N.name} is provided, the last ${N.name} items of the sequence will be omitted.
If there are fewer than ${N.name} in the sequence, the empty sequence is returned.
If a count ${N.name} is omitted, the last item of the sequence is omitted.

Examples:
   allButLast [1, 2, 3, 4, 5]   # [1, 2, 3, 4]
   allButLast 3 [1, 2, 3, 4, 5] # [1, 2]
   allButLast 5 [1, 2, 3]       # []""")
}
