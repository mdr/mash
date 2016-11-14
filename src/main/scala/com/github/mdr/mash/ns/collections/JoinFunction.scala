package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashString

object JoinFunction extends MashFunction("collections.join") {

  object Params {
    val Separator = Parameter(
      name = "separator",
      summary = """String used to separate elements in the result string; defaults to """"",
      defaultValueGeneratorOpt = Some(() ⇒ MashString("")))
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to combine into a string",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(Separator, Sequence))

  def apply(arguments: Arguments): MashString = {
    val boundParams = params.validate(arguments)
    val sequence = boundParams.validateSequence(Sequence)
    val separator = boundParams.validateString(Separator).s
    val chunks = sequence.map(ToStringifier.stringify)
    MashString(chunks.mkString(separator))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(StringClass)

  override def summary = "Combine the elements of a sequence into a string using a separator"

  override def descriptionOpt = Some("""Each element of the sequence will be converted into a string and joined with the given separator.

Examples:
   join ":" [1, 2, 3] # "1:2:3"
   join ":" []        # ""
	 join [1, 2, 3]     # "123"""")
}