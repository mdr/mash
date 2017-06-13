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
    boundParams(Sequence) match {
      case obj: MashObject ⇒
        ToListHelper.tryToList(obj) match {
          case Some(items) ⇒ MashList(items drop n)
          case None        ⇒ MashObject.of(obj.immutableFields drop n)
        }
      case s: MashString   ⇒ s.modify(_ drop n)
      case xs: MashList    ⇒ MashList(xs.immutableElements drop n)
      case value           ⇒
        boundParams.throwInvalidArgument(Sequence, s"Must be a List, String, or Object, but was a ${value.typeName}")
    }
  }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy(params, Sequence)

  override def summaryOpt = Some("Skip elements from the start of a sequence")

  override def descriptionOpt = Some("""If there are fewer elements in the sequence than are requested to 
    skip, the empty sequence is returned.
    
Examples:
  skip 2 [1, 2, 3, 4] # [3, 4]
  skip 3 [1, 2]       # []
  skip 3 "abcdef"     # "def"""")

}