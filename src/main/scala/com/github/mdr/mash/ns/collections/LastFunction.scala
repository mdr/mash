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
      descriptionOpt = Some("Must be a List, String or Object"))
  }

  import Params._

  val params = ParameterModel(N, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val countOpt = FirstFunction.validateCount(boundParams)
    boundParams(Sequence) match {
      case obj: MashObject ⇒
        ToListHelper.tryToList(obj) match {
          case Some(items) ⇒ last(items, countOpt)
          case None        ⇒ last(obj, countOpt)
        }
      case s: MashString   ⇒ last(s, countOpt)
      case xs: MashList    ⇒ last(xs.immutableElements, countOpt)
      case value           ⇒
        boundParams.throwInvalidArgument(Sequence, s"Must be a List, String, or Object, but was a ${value.typeName}")
    }
  }

  private def last(s: MashString, countOpt: Option[Int]): MashValue =
    countOpt match {
      case Some(count) ⇒ s.modify(_ takeRight count)
      case None        ⇒ if (s.isEmpty) MashNull else s.last
    }

  private def last(xs: Seq[MashValue], countOpt: Option[Int]): MashValue =
    countOpt match {
      case Some(count) ⇒ MashList(xs takeRight count)
      case None        ⇒ if (xs.isEmpty) MashNull else xs.last
    }

  private def last(obj: MashObject, countOpt: Option[Int]): MashValue =
    countOpt match {
      case Some(count) ⇒ MashObject.of(obj.immutableFields takeRight count)
      case None        ⇒ if (obj.isEmpty) MashNull else MashObject.of(obj.immutableFields takeRight 1)
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
  last 3 'abcdef'        # 'def'""")
}
