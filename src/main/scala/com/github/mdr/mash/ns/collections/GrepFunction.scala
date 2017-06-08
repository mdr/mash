package com.github.mdr.mash.ns.collections

import java.util.regex.Pattern

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.objectClass.GrepMethod
import com.github.mdr.mash.ns.core.{ AnyClass, StringClass }
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.utils.StringUtils

object GrepFunction extends MashFunction("collections.grep") {

  object Params {
    val Query = Parameter(
      nameOpt = Some("query"),
      summaryOpt = Some("Query to find in the given input"))
    val Input = Parameter(
      nameOpt = Some("input"),
      summaryOpt = Some("Sequence or string to search. A string will be treated as a sequence of lines."))
    val IgnoreCase = Parameter(
      nameOpt = Some("ignoreCase"),
      summaryOpt = Some("Perform a case-insensitive match"),
      shortFlagOpt = Some('i'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isBooleanFlag = true)
    val Negate = Parameter(
      nameOpt = Some("negate"),
      summaryOpt = Some("Find all items that don't match the given query"),
      shortFlagOpt = Some('n'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isBooleanFlag = true)
    val Regex = Parameter(
      nameOpt = Some("regex"),
      shortFlagOpt = Some('r'),
      summaryOpt = Some("Interpret query as a regular expression; otherwise, interpret query as the literal string (default false)"),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)

  }

  import Params._

  val params = ParameterModel(Query, Input, IgnoreCase, Regex, Negate)

  def call(boundParams: BoundParams): MashValue =
    boundParams(Input) match {
      case obj: MashObject ⇒
        GrepMethod.doGrep(obj, boundParams)
      case inSequence      ⇒
        val items = getInputItems(boundParams)
        val ignoreCase = boundParams(IgnoreCase).isTruthy
        val regex = boundParams(Regex).isTruthy
        val query = ToStringifier.stringify(boundParams(Query))
        val negate = boundParams(Negate).isTruthy
        runGrep(items, query, ignoreCase, regex, negate)
    }

  def getItems(s: MashString): Seq[MashString] = StringUtils.splitIntoLines(s.s).map(MashString(_, s.tagClassOpt))

  def runGrep(items: Seq[MashValue], query: String, ignoreCase: Boolean, regex: Boolean, negate: Boolean, ignoreFields: Boolean = true): MashList =
    MashList(items.filter(matches(_, query, ignoreCase, regex, negate, ignoreFields)))

  private def getInputItems(boundParams: BoundParams): Seq[MashValue] =
    boundParams(Input) match {
      case s: MashString ⇒ getItems(s)
      case xs: MashList  ⇒ xs.elements
      case input         ⇒ boundParams.throwInvalidArgument(Input, s"Expected a String or List, but was a '${input.typeName}'")
    }

  private def matches(value: MashValue, query: String, ignoreCase: Boolean, regex: Boolean, negate: Boolean, ignoreFields: Boolean): Boolean = {
    val valueString = value match {
      case obj: MashObject if ignoreFields ⇒ obj.immutableFields.values.mkString("\n")
      case _               ⇒ ToStringifier.stringify(value)
    }
    val valueMatches =
      if (regex) {
        val pattern = Pattern.compile(query, if (ignoreCase) Pattern.CASE_INSENSITIVE else 0)
        pattern.matcher(valueString).find()
      } else if (ignoreCase)
        valueString.toLowerCase contains query.toLowerCase
      else
        valueString contains query
    if (negate) !valueMatches else valueMatches
  }

  override object typeInferenceStrategy extends TypeInferenceStrategy {

    def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
      val inputTypeOpt = params.bindTypes(arguments).getType(Input)
      val preciseTypeOpt = inputTypeOpt.collect {
        case Type.Seq(elementType)                                        ⇒ Type.Seq(elementType)
        case s@(Type.Instance(StringClass) | Type.Tagged(StringClass, _)) ⇒ Type.Seq(s)
      }
      preciseTypeOpt orElse Some(Type.Seq(AnyClass))
    }

  }

  override def summaryOpt = Some("Find all the elements in the input which match the given query somewhere in its String representation")

  override def descriptionOpt = Some(
    """Examples:
  grep "b" ["apple", "book", "car"] # ["book"]""")

}
