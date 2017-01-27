package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.MashBoolean

object ContainsFunction extends MashFunction("collections.contains") {

  object Params {
    val Element = Parameter(
      nameOpt = Some("element"),
      summaryOpt = Some("Element to test for membership"))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to check whether it contains the given element"),
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Element, Sequence))

  def apply(arguments: Arguments): MashBoolean = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    val element = boundParams(Element)
    MashBoolean(sequence.contains(element))
  }

  override def typeInferenceStrategy = BooleanClass

  override def summaryOpt = Some("Check whether a sequence contains an element")

  override def descriptionOpt = Some("""Examples:
  contains 2 [1, 2, 3] # true
  contains 9 [1, 2, 3] # false""")

}