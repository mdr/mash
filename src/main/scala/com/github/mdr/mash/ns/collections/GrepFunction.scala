package com.github.mdr.mash.ns.collections

import java.util.regex.Pattern

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.objectClass.GrepMethod
import com.github.mdr.mash.ns.core.{ ObjectClass, StringClass }
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils._

object GrepFunction extends MashFunction("collections.grep") {

  object Params {
    val IgnoreCase = Parameter(
      nameOpt = Some("ignoreCase"),
      summaryOpt = Some("Perform a case-insensitive match (default false)"),
      shortFlagOpt = Some('i'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Regex = Parameter(
      nameOpt = Some("regex"),
      shortFlagOpt = Some('r'),
      summaryOpt = Some("Interpret query as a regular expression; otherwise, interpret query as the literal string (default false)"),
      defaultValueGeneratorOpt = Some(false),
      isFlag = true,
      isBooleanFlag = true)
    val Negate = Parameter(
      nameOpt = Some("negate"),
      summaryOpt = Some("Find all items that don't match the given query (default false)"),
      shortFlagOpt = Some('n'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val First = Parameter(
      nameOpt = Some("first"),
      summaryOpt = Some("Return the first match, if there is one, else null (default false)"),
      shortFlagOpt = Some('f'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Query = Parameter(
      nameOpt = Some("query"),
      summaryOpt = Some("Query to find in the given input"))
    val Input = Parameter(
      nameOpt = Some("input"),
      summaryOpt = Some("Sequence or string to search. A string will be treated as a sequence of lines."))

  }

  import Params._

  val params = ParameterModel(First, IgnoreCase, Negate, Regex, Query, Input)

  case class GrepOptions(ignoreCase: Boolean,
                         regex: Boolean,
                         negate: Boolean,
                         first: Boolean,
                         ignoreFields: Boolean)

  def call(boundParams: BoundParams): MashValue = {
    val query = ToStringifier.stringify(boundParams(Query))
    val ignoreCase = boundParams(IgnoreCase).isTruthy
    val regex = boundParams(Regex).isTruthy
    val negate = boundParams(Negate).isTruthy
    val first = boundParams(First).isTruthy
    val options = GrepOptions(ignoreCase, regex, negate, first, ignoreFields = true)
    SequenceLikeAnalyser.analyse(boundParams, Input) {
      case SequenceLike.List(items) ⇒ runGrep(items, query, options)
      case SequenceLike.String(s)   ⇒ runGrep(getItems(s), query, options)
      case SequenceLike.Object(obj) ⇒ GrepMethod.doGrep(obj, boundParams)
    }
  }

  def getItems(s: MashString): Seq[MashString] = StringUtils.splitIntoLines(s.s).map(MashString(_, s.tagClassOpt))

  def runGrep(items: Seq[MashValue], query: String, options: GrepOptions): MashValue =
    if (options.first)
      grepForFirst(items, query, options)
    else
      grepForAll(items, query, options)

  /**
    * Find the first matching item, if any, else MashNull
    */
  def grepForFirst(items: Seq[MashValue], query: String, options: GrepOptions): MashValue =
    items.find(matches(_, query, options)).getOrElse(MashNull)

  /**
    * Find all matching items, else the empty MashList
    */
  def grepForAll(items: Seq[MashValue], query: String, options: GrepOptions): MashList =
    MashList(items.filter(matches(_, query, options)))

  private def matches(value: MashValue, query: String, options: GrepOptions): Boolean = {
    import options._
    val valueString = value match {
      case obj: MashObject if ignoreFields ⇒ obj.immutableFields.values.mkString("\n")
      case _                               ⇒ ToStringifier.stringify(value)
    }
    val valueMatches =
      if (regex) {
        val pattern = Pattern.compile(query, if (ignoreCase) Pattern.CASE_INSENSITIVE else 0)
        pattern.matcher(valueString).find()
      } else if (ignoreCase)
        valueString.toLowerCase contains query.toLowerCase
      else
        valueString contains query
    valueMatches.when(negate, !_)
  }

  override def typeInferenceStrategy = GrepTypeInferenceStrategy

  override def summaryOpt = Some("Find all the elements in the input which match the given query somewhere in its String representation")

  override def descriptionOpt = Some(
    """If given a String, matching lines will be returned.

If given an Object, matching field/values will be retained.

Examples:
<mash>
  grep "b" ["apple", "book", "car"] # ["book"]
</mash>""")

}

object GrepTypeInferenceStrategy extends TypeInferenceStrategy {

  import GrepFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = GrepFunction.params.bindTypes(arguments)
    val first = argBindings.getBooleanValue(First).isDefined
    val inputTypeOpt = argBindings.getType(Input)
    val objectTypeOpt = inputTypeOpt.collect {
      case t@Type.Instance(klass) if klass isSubClassOf ObjectClass ⇒ t
      case t@(Type.Object(_) | Type.UserClassInstance(_))           ⇒ t
    }
    val resultType =
      if (objectTypeOpt.isDefined)
        Type.Instance(ObjectClass)
      else {
        val itemTypeOpt = inputTypeOpt.collect {
          case Type.Seq(elementType)                                        ⇒ elementType
          case t@(Type.Instance(StringClass) | Type.Tagged(StringClass, _)) ⇒ t
        }
        val itemType = itemTypeOpt getOrElse Type.Any
        if (first) itemType else itemType.seq
      }
    Some(resultType)
  }

}