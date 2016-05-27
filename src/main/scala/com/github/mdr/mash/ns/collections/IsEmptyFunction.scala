package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.ns.core.BooleanClass

object IsEmptyFunction extends MashFunction("collections.isEmpty") {

  object Params {
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence of elements to test")
  }

  import Params._

  val params = ParameterModel(Seq(Sequence))

  def apply(arguments: Arguments): Boolean = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)   
    sequence.isEmpty
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(BooleanClass)

  override def summary = "Check whether a given sequence is empty"

  override def descriptionOpt = Some("""Examples
  isEmpty []        # true
  isEmpty [1, 2, 3] # false""")

}

