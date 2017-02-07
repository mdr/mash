package com.github.mdr.mash.ns.core

import java.time.Instant
import java.util.regex.Pattern

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.collections.{ AnyFunction, _ }
import com.github.mdr.mash.ns.os.{ PathClass, PathSummaryClass }
import com.github.mdr.mash.ns.time.{ DateClass, DateTimeClass }
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashString, _ }
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils._
import com.joestelmach.natty.Parser

import scala.collection.JavaConverters._

object StringClass extends MashClass("core.String") {

  private val fileSystem = LinuxFileSystem

  override val methods = Seq(
    ListClass.methodise(AllFunction),
    ListClass.methodise(AnyFunction),
    ListClass.methodise(CountMatchesFunction),
    ListClass.methodise(EachFunction),
    ListClass.methodise(FlatMapFunction),
    ListClass.methodise(FindFunction),
    ListClass.methodise(FirstFunction),
    ListClass.methodise(GroupByFunction),
    ListClass.methodise(IndexOfFunction),
    ListClass.methodise(IsEmptyFunction),
    ListClass.methodise(JoinFunction),
    ListClass.methodise(LastFunction),
    ListClass.methodise(LengthFunction),
    ListClass.methodise(MapFunction),
    ListClass.methodise(MaxByFunction),
    ListClass.methodise(MaxFunction),
    ListClass.methodise(MinByFunction),
    ListClass.methodise(MinFunction),
    ListClass.methodise(NonEmptyFunction),
    ListClass.methodise(ReduceFunction),
    ListClass.methodise(ReverseFunction),
    ListClass.methodise(SkipFunction),
    ListClass.methodise(SkipUntilFunction),
    ListClass.methodise(SkipWhileFunction),
    ListClass.methodise(SlidingFunction),
    ListClass.methodise(SortByFunction),
    ListClass.methodise(SortFunction),
    ListClass.methodise(SumByFunction),
    ListClass.methodise(SumFunction),
    ListClass.methodise(TakeWhileFunction),
    ListClass.methodise(UniqueFunction),
    ListClass.methodise(WhereFunction),
    ListClass.methodise(WhereNotFunction),
    ContainsMethod,
    EndsWithMethod,
    GlobMethod,
    GrepMethod,
    LinesMethod,
    MatchesMethod,
    RMethod,
    ReplaceMethod,
    StartsWithMethod,
    SplitMethod,
    TagMethod,
    ToDateMethod,
    ToDateTimeMethod,
    ToLowerMethod,
    ToNumberMethod,
    ToPathMethod,
    ToUpperMethod,
    TrimMethod,
    UntaggedMethod,
    MashClass.alias("g", GlobMethod),
    MashClass.alias("count", ListClass.methodise(LengthFunction)))

  object ContainsMethod extends MashMethod("contains") {

    object Params {
      val Substring = Parameter(
        nameOpt = Some("substring"),
        summaryOpt = Some("Substring to match"))
    }

    import Params._

    val params = ParameterModel(Seq(Substring))

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      val boundParams = params.validate(arguments)
      val s = target.asInstanceOf[MashString].s
      val pattern = ToStringifier.stringify(boundParams(Substring))
      MashBoolean(s contains pattern)
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("Test whether this string contains the given substring")

  }

  object GlobMethod extends MashMethod("glob") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      val pattern = target.asInstanceOf[MashString].s
      MashList(fileSystem.glob(pattern).map(PathSummaryClass.asMashObject))
    }

    override def typeInferenceStrategy = Seq(PathSummaryClass)

    override def summaryOpt = Some("Return paths matching a glob pattern")

  }

  object TrimMethod extends MashMethod("trim") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      val boundParams = params.validate(arguments)
      val s = target.asInstanceOf[MashString]
      s.modify(_.trim)
    }

    override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

    override def summaryOpt = Some("Strip initial and trailing whitespace")

  }

  object RMethod extends MashMethod("r") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      target.asInstanceOf[MashString].copy(tagClassOpt = Some(RegexClass))
    }

    override def typeInferenceStrategy = Type.Tagged(StringClass, RegexClass)

    override def summaryOpt = Some("This string as a regular expression")

  }

  object TagMethod extends MashMethod("tag") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      target.asInstanceOf[MashString].tagClassOpt.getOrElse(MashNull)
    }

    override def typeInferenceStrategy = ClassClass

    override def summaryOpt = Some("This string's tagged type, if any")
  }

  object UntaggedMethod extends MashMethod("untagged") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      target.asInstanceOf[MashString].copy(tagClassOpt = None)
    }

    override def typeInferenceStrategy = StringClass

    override def summaryOpt = Some("This string without any tag class")
  }

  object MatchesMethod extends MashMethod("matches") {

    object Params {
      val _Pattern = Parameter(
        nameOpt = Some("pattern"),
        summaryOpt = Some("Regular expression pattern"))
      val IgnoreCase = Parameter(
        nameOpt = Some("ignoreCase"),
        summaryOpt = Some("Perform a case-insensitive match"),
        shortFlagOpt = Some('i'),
        isFlag = true,
        defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
        isBooleanFlag = true)
    }

    import Params._

    val params = ParameterModel(Seq(_Pattern, IgnoreCase))

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      val boundParams = params.validate(arguments)
      val s = target.asInstanceOf[MashString].s
      val pattern = ToStringifier.stringify(boundParams(_Pattern))
      val ignoreCase = boundParams(IgnoreCase).isTruthy
      val flags = if (ignoreCase) Pattern.CASE_INSENSITIVE else 0
      MashBoolean(Pattern.compile(pattern, flags).matcher(s).find)
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("Test whether this string contains a match within it to a given regular expression")

  }

  object GrepMethod extends MashMethod("grep") {
    import GrepFunction.Params._

    val params = ParameterModel(Seq(Query, IgnoreCase, Regex))

    override def apply(target: MashValue, arguments: Arguments): MashValue = {
      val boundParams = params.validate(arguments)
      val ignoreCase = boundParams(IgnoreCase).isTruthy
      val regex = boundParams(Regex).isTruthy
      val query = ToStringifier.stringify(boundParams(Query))
      val items = GrepFunction.getItems(target.asInstanceOf[MashString])
      GrepFunction.runGrep(items, query, ignoreCase, regex)
    }

    override object typeInferenceStrategy extends MethodTypeInferenceStrategy {
      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
        targetTypeOpt.map(_.seq)
    }

    override def summaryOpt: Option[String] = Some("Find all the elements in the lines of this String which match the given query")
  }

  object LinesMethod extends MashMethod("lines") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      val targetString = target.asInstanceOf[MashString]
      val pieces = StringUtils.splitIntoLines(targetString.s)
      MashList(pieces.map(MashString(_, targetString.tagClassOpt)))
    }

    override def typeInferenceStrategy = SplitMethod.typeInferenceStrategy

    override def summaryOpt = Some("Split this string into a sequence of lines")

  }

  object SplitMethod extends MashMethod("split") {

    object Params {
      val Regex = Parameter(
        nameOpt = Some("regex"),
        shortFlagOpt = Some('r'),
        summaryOpt = Some("Interpret separator as a regular expression; otherwise, interpret separator as the literal string (default false)"),
        defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
        isFlag = true,
        isBooleanFlag = true)
      val Separator = Parameter(
        nameOpt = Some("separator"),
        summaryOpt = Some("Separator to split string on; if not provided, the default is to split on whitespace"),
        defaultValueGeneratorOpt = Some(() ⇒ MashNull))
    }

    import Params._

    val params = ParameterModel(Seq(Regex, Separator))

    def apply(target: MashValue, arguments: Arguments): MashList = {
      val boundParams = params.validate(arguments)
      val targetString = target.asInstanceOf[MashString]
      val regex = boundParams(Regex).isTruthy
      val separator = getSeparator(boundParams, Separator, regex)
      split(targetString, separator)
    }

    def split(string: MashString, separator: String): MashList = {
      val pieces = string.s.split(separator, -1)
      MashList(pieces.map(MashString(_, string.tagClassOpt)))
    }

    override def typeInferenceStrategy = (inferencer, targetTypeOpt, arguments) =>
      targetTypeOpt orElse Some(Type.Instance(StringClass)) map (_.seq)

    override def summaryOpt = Some("Split this string into a sequence of substrings using a separator")

    def getSeparator(boundParams: BoundParams, param: Parameter, regex: Boolean): String =
      boundParams(Separator) match {
        case MashNull                 ⇒ "\\s+"
        case MashString(separator, _) ⇒ if (regex) separator else Pattern.quote(separator)
        case x                        ⇒ boundParams.throwInvalidArgument(Separator, "Invalid separator of type " + x.typeName)
      }
  }


  object ReplaceMethod extends MashMethod("replace") {

    object Params {
      val Target = Parameter(
        nameOpt = Some("target"),
        summaryOpt = Some("String to replace"))
      val Replacement = Parameter(
        nameOpt = Some("replacement"),
        summaryOpt = Some("Replacement string"))
      val Regex = Parameter(
        nameOpt = Some("regex"),
        shortFlagOpt = Some('r'),
        summaryOpt = Some("Interpret target as a regular expression"),
        defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
        isFlag = true,
        isBooleanFlag = true)

    }

    import Params._

    val params = ParameterModel(Seq(Target, Replacement, Regex))

    def apply(target: MashValue, arguments: Arguments): MashString = {
      val boundParams = params.validate(arguments)
      val s = target.asInstanceOf[MashString]
      val regex = boundParams(Regex).isTruthy
      val targetString = boundParams.validateString(Target).s
      val replacement = boundParams.validateString(Replacement).s
      if (regex)
        s.modify(_.replaceAll(targetString, replacement))
      else
        s.modify(_.replace(targetString, replacement))
    }

    override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

    override def summaryOpt = Some("Replace occurrences of a string with another")

  }

  object LengthMethod extends MashMethod("length") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashString].length
    }

    override def typeInferenceStrategy = NumberClass

    override def summaryOpt = Some("Length of this string")

  }

  object ToLowerMethod extends MashMethod("toLower") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      target.asInstanceOf[MashString].modify(_.toLowerCase)
    }

    override def summaryOpt = Some("Convert string to lowercase")

    override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

  }

  object ToUpperMethod extends MashMethod("toUpper") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      target.asInstanceOf[MashString].modify(_.toUpperCase)
    }

    override def summaryOpt = Some("Convert string to uppercase")

    override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

  }

  object ToNumberMethod extends MashMethod("toNumber") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      MashNumber(target.asInstanceOf[MashString].s.toDouble)
    }

    override def summaryOpt = Some("Parse this string as a number")

    override def typeInferenceStrategy = NumberClass

  }

  object ToDateTimeMethod extends MashMethod("toDateTime") {

    val params = ParameterModel()

    private val parser = new Parser

    def parseInstant(s: String): Option[Instant] =
      parser.parse(s).asScala.headOption.flatMap(_.getDates.asScala.headOption).map(_.toInstant)

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      parseInstant(target.asInstanceOf[MashString].s).map(MashWrapped).getOrElse(MashNull)
    }

    override def summaryOpt = Some("Parse this string as a DateTime")

    override def typeInferenceStrategy = DateTimeClass

  }

  object ToDateMethod extends MashMethod("toDate") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      ToDateTimeMethod.parseInstant(target.asInstanceOf[MashString].s)
        .map(DateTimeClass.DateMethod.toLocalDate)
        .map(MashWrapped)
        .getOrElse(MashNull)
    }

    override def summaryOpt = Some("Parse this string as a Date")

    override def typeInferenceStrategy = DateClass

  }

  object ToPathMethod extends MashMethod("toPath") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      target.asInstanceOf[MashString].copy(tagClassOpt = Some(PathClass))
    }

    override def summaryOpt = Some("Tag this string as a path")

    override def typeInferenceStrategy = StringClass taggedWith PathClass

  }

  object StartsWithMethod extends MashMethod("startsWith") {

    object Params {
      val Prefix = Parameter(
        nameOpt = Some("prefix"),
        summaryOpt = Some("Prefix to test"))
    }

    import Params._

    val params = ParameterModel(Seq(Prefix))

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      val boundParams = params.validate(arguments)
      val s = target.asInstanceOf[MashString]
      val pattern = boundParams(Prefix).asInstanceOf[MashString]
      MashBoolean(s.startsWith(pattern))
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("Check if this string starts with another")

  }

  object EndsWithMethod extends MashMethod("endsWith") {

    object Params {
      val Suffix = Parameter(
        nameOpt = Some("suffix"),
        summaryOpt = Some("Suffix to test"))
    }

    import Params._

    val params = ParameterModel(Seq(Suffix))

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      val boundParams = params.validate(arguments)
      val s = target.asInstanceOf[MashString]
      val pattern = boundParams(Suffix).asInstanceOf[MashString]
      MashBoolean(s.reverse.startsWith(pattern.reverse))
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("Check if this string ends with another")

  }

  override def summaryOpt = Some("A string")

  def taggedWith(klass: MashClass) = Type.Tagged(this, klass)

  override def parentOpt = Some(AnyClass)

}

object SameStringMethodTypeInferenceStrategy extends MethodTypeInferenceStrategy {

  override def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
    targetTypeOpt.orElse(Some(Type.Instance(StringClass)))

}