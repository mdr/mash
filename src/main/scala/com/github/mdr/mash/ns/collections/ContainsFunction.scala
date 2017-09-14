package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.MashBoolean

object ContainsFunction extends MashFunction("collections.contains") {

  object Params {
    val Element = Parameter(
      nameOpt = Some("element"),
      summaryOpt = Some("Element to test for membership"))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to check whether it contains the given element"))
  }
  import Params._

  val params = ParameterModel(Element, Sequence)

  def call(boundParams: BoundParams): MashBoolean = {
    val sequence = boundParams.validateSequence(Sequence)
    val element = boundParams(Element)
    MashBoolean(sequence.contains(element))
  }

  override def typeInferenceStrategy = Boolean

  override def summaryOpt = Some("Check whether a sequence contains an element")

  override def descriptionOpt = Some("""Examples:
<mash>
  contains 2 [1, 2, 3] # true
  contains 9 [1, 2, 3] # false
</mash>""")

}