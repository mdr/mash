package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.MashBoolean

object NonEmptyFunction extends MashFunction("collections.nonEmpty") {

  import IsEmptyFunction.Params._

  val params = IsEmptyFunction.params

  def call(boundParams: BoundParams): MashBoolean = {
    val sequence = boundParams.validateSequence(Sequence)
    MashBoolean(sequence.nonEmpty)
  }

  override def typeInferenceStrategy = Boolean

  override def summaryOpt = Some("Check whether a given sequence is non-empty")

  override def descriptionOpt = Some("""Examples:
<mash>
  isEmpty []        # false
  isEmpty [1, 2, 3] # true
</mash>""")

}

