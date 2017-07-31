package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashString

object JoinFunction extends MashFunction("collections.join") {

  object Params {
    val Separator = Parameter(
      nameOpt = Some("separator"),
      summaryOpt = Some("""String used to separate elements in the result string; defaults to """""),
      defaultValueGeneratorOpt = Some(MashString("")))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to combine into a string"))
  }
  import Params._

  val params = ParameterModel(Separator, Sequence)

  def call(boundParams: BoundParams): MashString = {
    val sequence = boundParams.validateSequence(Sequence)
    val separator = boundParams.validateString(Separator).s
    val chunks = sequence.map(ToStringifier.stringify)
    MashString(chunks.mkString(separator))
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("Combine the elements of a sequence into a string using a separator")

  override def descriptionOpt = Some("""Each element of the sequence will be converted into a string and joined with the given separator.

Examples:
<mash>
  join ":" [1, 2, 3] # "1:2:3"
  join ":" []        # ""
  join [1, 2, 3]     # "123"
</mash>""")
}