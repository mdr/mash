package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.runtime.{ MashNull, MashNumber, MashValue }

object IndexOfFunction extends MashFunction("collections.indexOf") {

  object Params {
    val Element = Parameter(
      nameOpt = Some("element"),
      summary = "Element to test for membership")
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summary = "Sequence to check whether it contains the given element",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Element, Sequence))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    val element = boundParams(Element)
    sequence.indexOf(element) match {
      case -1 ⇒ MashNull
      case n ⇒ MashNumber(n)
    }
  }

  override def typeInferenceStrategy = NumberClass

  override def summary = "Return the index of the first occurrence of the given element, if any, else null"

  override def descriptionOpt = Some("""Examples:
  indexOf 2 [1, 2, 3] # 1
  indexOf 9 [1, 2, 3] # null""")

}