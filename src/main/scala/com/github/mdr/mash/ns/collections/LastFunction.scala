package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashNumber
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.evaluator.MashList

object LastFunction extends MashFunction("collections.last") {

  object Params {
    val N: Parameter = Parameter(
      name = "n",
      summary = "Number of elements to take",
      defaultValueGeneratorOpt = Some(() ⇒ null))
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to find the last value(s) of",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(N, Sequence))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams(Sequence)
    val countOpt = boundParams.validateIntegerOrNull(N)
    countOpt match {
      case Some(n) ⇒
        sequence match {
          case s: MashString ⇒ s.modify(_.takeRight(n))
          case xs: MashList  ⇒ MashList(xs.items.takeRight(n))
        }
      case None ⇒
        sequence match {
          case s: MashString ⇒ if (s.isEmpty) null else s.last
          case xs: MashList  ⇒ if (xs.isEmpty) null else xs.last
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
