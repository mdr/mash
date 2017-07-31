package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.SeqToSeqTypeInferenceStrategy
import com.github.mdr.mash.runtime._

object SkipFunction extends MashFunction("collections.skip") {

  object Params {
    val N = Parameter(
      nameOpt = Some("n"),
      summaryOpt = Some("Number of elements to skip (default 1)"),
      defaultValueGeneratorOpt = Some(MashNumber(1)))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to skip values from"),
      descriptionOpt = Some("Can be a List, String or Object"))
  }

  import Params._

  val params = ParameterModel(N, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val n = boundParams.validateNonNegativeInteger(N)
    SequenceLikeAnalyser.analyse(boundParams, Sequence) {
      case SequenceLike.List(items) ⇒ MashList(items drop n)
      case SequenceLike.String(s)   ⇒ s.modify(_ drop n)
      case SequenceLike.Object(obj) ⇒ MashObject.of(obj.immutableFields drop n)
    }
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy(params, Sequence)

  override def summaryOpt = Some("Skip elements from the start of a sequence")

  override def descriptionOpt = Some(
    """If there are fewer elements in the sequence than are requested to
  skip, the empty sequence is returned.

Examples:
<mash>
  skip 2 [1, 2, 3, 4] # [3, 4]
  skip 3 [1, 2]       # []
  skip 3 "abcdef"     # "def"
</mash>""")

}