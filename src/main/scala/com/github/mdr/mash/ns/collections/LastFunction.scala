package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.NoArgFunction._
import com.github.mdr.mash.runtime._

object LastFunction extends MashFunction("collections.last") {

  object Params {
    val N = Parameter(
      nameOpt = Some("n"),
      summaryOpt = Some("Number of elements to take"),
      defaultValueGeneratorOpt = Some(NoArgValue))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to find the last value(s) of"),
      descriptionOpt = Some("Can be a List, String or Object"))
  }

  import Params._

  val params = ParameterModel(N, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val countOpt = boundParams.validateNonNegativeIntegerOpt(N)
    SequenceLikeAnalyser.analyse(boundParams, Sequence) {
      case SequenceLike.Items(items) ⇒ countOpt match {
        case Some(count) ⇒ MashList(items takeRight count)
        case None        ⇒ if (items.isEmpty) MashNull else items.last
      }
      case SequenceLike.String(s)    ⇒ countOpt match {
        case Some(count) ⇒ s.modify(_ takeRight count)
        case None        ⇒ if (s.isEmpty) MashNull else s.last
      }
      case SequenceLike.Object(obj)  ⇒ countOpt match {
        case Some(count) ⇒ MashObject.of(obj.immutableFields takeRight count)
        case None        ⇒ if (obj.isEmpty) MashNull else MashObject.of(obj.immutableFields takeRight 1)
      }
    }
  }

  override def typeInferenceStrategy = FirstLastTypeInferenceStrategy(params, Sequence, N)

  override def summaryOpt = Some("Find the last element(s) of a sequence")

  override def descriptionOpt = Some(
    s"""If a count ${N.name} is provided, the last ${N.name} items of the sequence will be returned.
If there are fewer than ${N.name} in the sequence, the entire sequence is returned.
If a count ${N.name} is omitted, then the last item of the sequence is returned, if nonempty, else null.

Examples:
  last 3 [1, 2, 3, 4, 5] # [3, 4, 5]
  last 5 [1, 2, 3]       # [1, 2, 3]
  last [1, 2, 3]         # 3
  last []                # null
  last 3 "abcdef"        # "def""")
}
