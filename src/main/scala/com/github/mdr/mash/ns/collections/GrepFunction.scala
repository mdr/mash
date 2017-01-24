package com.github.mdr.mash.ns.collections

import java.util.regex.Pattern

import com.github.mdr.mash.evaluator.{ Arguments, ToStringifier }
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashBoolean, MashList, MashString, MashValue }

object GrepFunction extends MashFunction("collections.grep") {

  object Params {
    val Query = Parameter(
      nameOpt = Some("query"),
      summaryOpt = Some("Query to find in the given sequence"))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to find values in"),
      isLast = true)
    val IgnoreCase = Parameter(
      nameOpt = Some("ignoreCase"),
      summaryOpt = Some("Perform a case-insensitive match"),
      shortFlagOpt = Some('i'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
    val Regex = Parameter(
      nameOpt = Some("regex"),
      shortFlagOpt = Some('r'),
      summaryOpt = Some("Interpret query as a regular expression; otherwise, interpret query as the literal string (default false)"),
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)

  }
  import Params._

  val params = ParameterModel(Seq(Query, Sequence, IgnoreCase, Regex))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val ignoreCase = boundParams(IgnoreCase).isTruthy
    val regex = boundParams(Regex).isTruthy
    val query = ToStringifier.stringify(boundParams(Query))
    val newSequence = sequence.filter(matches(_, query, ignoreCase, regex))
    reassembleSequence(inSequence, newSequence)
  }

  private def matches(value: MashValue, query: String, ignoreCase: Boolean, regex: Boolean): Boolean = {
    val valueString = ToStringifier.stringify(value)
    if (regex) {
      val pattern = Pattern.compile(query, if (ignoreCase) Pattern.CASE_INSENSITIVE else 0)
      pattern.matcher(valueString).find()
    } else if (ignoreCase)
      valueString.toLowerCase contains query.toLowerCase
    else
      valueString contains query
  }

  def reassembleSequence(inSequence: MashValue, newSequence: Seq[_ <: MashValue]): MashValue =
    inSequence match {
      case MashString(_, tagOpt) ⇒ newSequence.asInstanceOf[Seq[MashString]].fold(MashString("", tagOpt))(_ + _)
      case _                     ⇒ MashList(newSequence)
    }

  override def typeInferenceStrategy = SeqToSeqTypeInferenceStrategy

  override def summaryOpt = Some("Find all the elements in the sequence which match the given query somewhere in its String representation")

  override def descriptionOpt = Some("""Examples:
  grep "b" ["apple", "book", "car"] # ["book"]""")

}
