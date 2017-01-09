package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.{ Arguments, ToStringifier }
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashBoolean, MashList, MashString, MashValue }

object GrepFunction extends MashFunction("collections.grep") {

  object Params {
    val Query = Parameter(
      nameOpt = Some("query"),
      summary = "Query to find in the given sequence")
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summary = "Sequence to find values in",
      isLast = true)
    val IgnoreCase = Parameter(
      nameOpt = Some("ignoreCase"),
      summary = "Perform a case-insensitive match",
      shortFlagOpt = Some('i'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Query, Sequence, IgnoreCase))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val ignoreCase = boundParams(IgnoreCase).isTruthy
    val query = ToStringifier.stringify(boundParams(Query))
    val newSequence = sequence.filter(matches(_, query, ignoreCase))
    reassembleSequence(inSequence, newSequence)
  }

  private def matches(value: MashValue, query: String, ignoreCase: Boolean): Boolean = {
    val valueString = ToStringifier.stringify(value)
    if (ignoreCase)
      valueString.toLowerCase.contains(query.toLowerCase)
    else
      valueString.contains(query)
  }

  def reassembleSequence(inSequence: MashValue, newSequence: Seq[_ <: MashValue]): MashValue =
    inSequence match {
      case MashString(_, tagOpt) ⇒ newSequence.asInstanceOf[Seq[MashString]].fold(MashString("", tagOpt))(_ + _)
      case _                     ⇒ MashList(newSequence)
    }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy

  override def summary = "Find all the elements in the sequence which match the given query somewhere in its String representation"

  override def descriptionOpt = Some("""Examples:
  grep "b" ["apple", "book", "car"] # ["book"]""")

}
