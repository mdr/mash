package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.MashBoolean

object IsEmptyFunction extends MashFunction("collections.isEmpty") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence of elements to test"))
  }

  import Params._

  val params = ParameterModel(Sequence)

  def call(boundParams: BoundParams): MashBoolean = {
    val sequence = boundParams.validateSequence(Sequence)
    MashBoolean(sequence.isEmpty)
  }

  override def typeInferenceStrategy = Boolean

  override def summaryOpt = Some("Check whether a given sequence is empty")

  override def descriptionOpt = Some("""Examples:
<mash>
  isEmpty []        # true
  isEmpty [1, 2, 3] # false
</mash>""")

}

