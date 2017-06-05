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
      summaryOpt = Some("Sequence to find the value(s) of"))
  }

  import Params._

  val params = ParameterModel(Seq(N, Sequence))

  def call(boundParams: BoundParams): MashValue = {
    boundParams.validateSequence(Sequence)
    val sequence = boundParams(Sequence)
    val count = boundParams.validateInteger(N)
    if (count < 0)
      boundParams.throwInvalidArgument(N, s"Must be non-negative, but was $count")
    else
      sequence match {
        case s: MashString ⇒ s.modify(_ dropRight count)
        case xs: MashList  ⇒ MashList(xs.elements dropRight count)
      }
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy(params, Sequence)

  override def summaryOpt = Some("Take all but the last element(s) of a sequence")

  override def descriptionOpt = Some(
    s"""If a count ${N.nameOpt} is provided, the last ${N.nameOpt} items of the sequence will be omitted.
If there are fewer than ${N.nameOpt} in the sequence, the the empty sequence is returned.
If a count ${N.nameOpt} is omitted, then the last item of the sequence is omitted.

Examples:
   allButLast [1, 2, 3, 4, 5]   # [1, 2, 3, 4]
   allButLast 3 [1, 2, 3, 4, 5] # [1, 2]
   allButLast 5 [1, 2, 3]       # []""")
}
