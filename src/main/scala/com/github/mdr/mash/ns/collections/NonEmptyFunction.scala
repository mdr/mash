package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.MashBoolean

object NonEmptyFunction extends MashFunction("collections.nonEmpty") {

  import IsEmptyFunction.Params._

  val params = IsEmptyFunction.params

  def apply(arguments: Arguments): MashBoolean = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    MashBoolean(sequence.nonEmpty)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(BooleanClass)

  override def summary = "Check whether a given sequence is non-empty"

  override def descriptionOpt = Some("""Examples
  isEmpty []        # false
  isEmpty [1, 2, 3] # true""")

}

