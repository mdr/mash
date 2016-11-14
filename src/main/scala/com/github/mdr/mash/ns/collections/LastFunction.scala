package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.functions._
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashValue

object LastFunction extends MashFunction("collections.last") {

  object Params {
    val N: Parameter = Parameter(
      name = "n",
      summary = "Number of elements to take",
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to find the last value(s) of",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(N, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams(Sequence)
    boundParams.validateIntegerOpt(N) match {
      case Some(count) ⇒
        sequence match {
          case s: MashString ⇒ s.modify(_ takeRight count)
          case xs: MashList  ⇒ MashList(xs.items takeRight count)
        }
      case None ⇒
        sequence match {
          case s: MashString ⇒ if (s.isEmpty) MashNull else s.last
          case xs: MashList  ⇒ if (xs.isEmpty) MashNull else xs.last
        }
    }
  }

  override def typeInferenceStrategy = FirstTypeInferenceStrategy

  override def summary = "Find the last element(s) of a sequence"

  override def descriptionOpt = Some(s"""If a count ${N.name} is provided, the last ${N.name} items of the sequence will be returned.
If there are fewer than ${N.name} in the sequence, the entire sequence is returned.
If a count ${N.name} is omitted, then the last item of the sequence is returned, if nonempty, else null.

Examples:
   last 3 [1, 2, 3, 4 5] # [3, 4, 5]
   last 5 [1, 2, 3]      # [1, 2, 3]
   last [1, 2, 3]        # 3
   last []               # null""")
}
