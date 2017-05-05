package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.NoArgFunction._
import com.github.mdr.mash.runtime.{ MashList, MashNull, MashString, MashValue }

object LastFunction extends MashFunction("collections.last") {

  object Params {
    val N = Parameter(
      nameOpt = Some("n"),
      summaryOpt = Some("Number of elements to take"),
      defaultValueGeneratorOpt = Some(NoArgValue))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to find the last value(s) of"),
      isLast = true)
  }

  import Params._

  val params = ParameterModel(Seq(N, Sequence))

  def call(boundParams: BoundParams): MashValue = {
    boundParams.validateSequence(Sequence)
    val sequence = boundParams(Sequence)
    boundParams.validateIntegerOpt(N) match {
      case Some(count) ⇒
        if (count < 0)
          boundParams.throwInvalidArgument(N, s"Must be non-negative, but was $count")
        else
          sequence match {
            case s: MashString ⇒ s.modify(_ takeRight count)
            case xs: MashList  ⇒ MashList(xs.elements takeRight count)
          }
      case None        ⇒
        sequence match {
          case s: MashString ⇒ if (s.isEmpty) MashNull else s.last
          case xs: MashList  ⇒ if (xs.isEmpty) MashNull else xs.last
        }
    }
  }

  override def typeInferenceStrategy = FirstTypeInferenceStrategy

  override def summaryOpt = Some("Find the last element(s) of a sequence")

  override def descriptionOpt = Some(
    s"""If a count ${N.nameOpt} is provided, the last ${N.nameOpt} items of the sequence will be returned.
If there are fewer than ${N.nameOpt} in the sequence, the entire sequence is returned.
If a count ${N.nameOpt} is omitted, then the last item of the sequence is returned, if nonempty, else null.

Examples:
   last 3 [1, 2, 3, 4, 5] # [3, 4, 5]
   last 5 [1, 2, 3]       # [1, 2, 3]
   last [1, 2, 3]         # 3
   last []                # null""")
}
