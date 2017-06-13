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
    boundParams(Sequence) match {
      case obj: MashObject ⇒
        ToListHelper.tryToList(obj) match {
          case Some(items) ⇒ MashList(items dropRight n)
          case None        ⇒ MashObject.of(obj.immutableFields dropRight n)
        }
      case s: MashString   ⇒ s.modify(_ dropRight n)
      case xs: MashList    ⇒ MashList(xs.immutableElements dropRight n)
      case value           ⇒
        boundParams.throwInvalidArgument(Sequence, s"Must be a List, String, or Object, but was a ${value.typeName}")
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
