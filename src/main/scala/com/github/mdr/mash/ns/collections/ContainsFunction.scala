package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.functions.FunctionHelpers

object ContainsFunction extends MashFunction("collections.contains") {

  object Params {
    val Element = Parameter(
      name = "element",
      summary = "Element to test for membership")
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to check whether it contains the given element",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Element, Sequence))

  def apply(arguments: Arguments): Boolean = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)   
    val element = boundParams(Element)
    sequence.contains(element)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(BooleanClass)

  override def summary = "Check whether a sequence contains an element"

  override def descriptionOpt = Some("""Examples:
  contains 2 [1, 2, 3] # true
  contains 9 [1, 2, 3] # false""")

}