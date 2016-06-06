package com.github.mdr.mash.ns.core

import java.util.regex.Pattern
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.ns.os.PathSummaryClass
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.runtime.MashBoolean

object StringClass extends MashClass("core.String") {

  private val fileSystem = LinuxFileSystem
  private val envInteractions = LinuxEnvironmentInteractions

  override val methods = Seq(
    FirstMethod,
    GlobMethod,
    IsEmptyMethod,
    MatchesMethod,
    LengthMethod,
    LastMethod,
    NonEmptyMethod,
    RMethod,
    ReplaceMethod,
    ReverseMethod,
    StartsWithMethod,
    SplitMethod,
    TagMethod,
    ToLowerMethod,
    ToNumberMethod,
    ToPathMethod,
    ToUpperMethod,
    TrimMethod,
    UntaggedMethod,
    MashClass.alias("g", GlobMethod))

  object GlobMethod extends MashMethod("glob") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashList = {
      params.validate(arguments)
      val pattern = target.asInstanceOf[MashString].s
      MashList(fileSystem.glob(pattern).map(PathSummaryClass.asMashObject))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Seq(PathSummaryClass))

    override def summary = "Return paths matching a glob pattern"

  }

  object IsEmptyMethod extends MashMethod("isEmpty") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      MashBoolean(target.asInstanceOf[MashString].s.isEmpty)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Return true if this is the empty string"

  }

  object NonEmptyMethod extends MashMethod("nonEmpty") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      MashBoolean(target.asInstanceOf[MashString].s.nonEmpty)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Return true if this is a non-empty string"

  }

  object TrimMethod extends MashMethod("trim") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      val boundParams = params.validate(arguments)
      val s = target.asInstanceOf[MashString]
      s.modify(_.trim)
    }

    override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

    override def summary = "Strip initial and trailing whitespace"

  }

  object RMethod extends MashMethod("r") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      target.asInstanceOf[MashString].copy(tagClassOpt = Some(RegexClass))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, RegexClass))

    override def summary = "This string as a regular expression"

  }

  object TagMethod extends MashMethod("tag") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashValue = {
      params.validate(arguments)
      target.asInstanceOf[MashString].tagClassOpt.getOrElse(MashNull)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(ClassClass)

    override def summary = "This string's tagged type, if any"
  }

  object UntaggedMethod extends MashMethod("untagged") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      target.asInstanceOf[MashString].copy(tagClassOpt = None)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

    override def summary = "This string without any tag class"
  }

  object MatchesMethod extends MashMethod("matches") {

    object Params {
      val _Pattern = Parameter(
        name = "pattern",
        summary = "Regular expression pattern")
    }
    import Params._

    val params = ParameterModel(Seq(_Pattern))

    def apply(target: Any, arguments: Arguments): MashBoolean = {
      val boundParams = params.validate(arguments)
      val s = target.asInstanceOf[MashString].s
      val pattern = boundParams(_Pattern).asInstanceOf[MashString].s
      MashBoolean(Pattern.compile(pattern).matcher(s).find)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Test whether this string contains a match within it to a given regular expression"

  }

  object SplitMethod extends MashMethod("split") {

    object Params {
      val Regex = Parameter(
        name = "regex",
        shortFlagOpt = Some('r'),
        summary = "Interpret separator as a regular expression",
        defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
        isFlag = true,
        isBooleanFlag = true)
      val Separator = Parameter(
        name = "separator",
        summary = "Separator to split string on",
        defaultValueGeneratorOpt = Some(() ⇒ MashNull))
    }
    import Params._

    val params = ParameterModel(Seq(
      Regex,
      Separator))

    def apply(target: Any, arguments: Arguments): MashList = {
      val boundParams = params.validate(arguments)
      val targetString = target.asInstanceOf[MashString]
      val regex = Truthiness.isTruthy(boundParams(Regex))
      val pieces = boundParams(Separator) match {
        case MashNull ⇒
          targetString.s.split("\\s+")
        case MashString(separator, _) ⇒
          val delimiterPattern = if (regex) separator else Pattern.quote(separator)
          targetString.s.split(delimiterPattern)
        case _ ⇒
          throw new EvaluatorException("Invalid separator")
      }
      MashList(pieces.map(piece ⇒ MashString(piece, targetString.tagClassOpt)))
    }

    override def typeInferenceStrategy = new MethodTypeInferenceStrategy {

      override def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
        targetTypeOpt.orElse(Some(Type.Instance(StringClass))).map(Type.Seq)

    }

    override def summary = "Split this string into a sequence of substrings using a separator"

  }

  object ReplaceMethod extends MashMethod("replace") {

    object Params {
      val Target = Parameter(
        name = "target",
        "String to replace")
      val Replacement = Parameter(
        "replacement",
        "Replacement string")
    }
    import Params._

    val params = ParameterModel(Seq(Target, Replacement))

    def apply(target: Any, arguments: Arguments): MashString = {
      val boundParams = params.validate(arguments)
      val s = target.asInstanceOf[MashString]
      val targetString = boundParams(Target).asInstanceOf[MashString].s
      val replacement = boundParams(Replacement).asInstanceOf[MashString].s
      s.modify(_.replace(targetString, replacement))
    }

    override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

    override def summary = "Replace occurrences of a string with another"

  }

  object LengthMethod extends MashMethod("length") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashString].length
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(NumberClass)

    override def summary = "Length of this string"

  }

  object ToLowerMethod extends MashMethod("toLower") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      target.asInstanceOf[MashString].modify(_.toLowerCase)
    }

    override def summary = "Convert string to lowercase"

    override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

  }

  object ToUpperMethod extends MashMethod("toUpper") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      target.asInstanceOf[MashString].modify(_.toUpperCase)
    }

    override def summary = "Convert string to uppercase"

    override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

  }

  object ToNumberMethod extends MashMethod("toNumber") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      MashNumber(target.asInstanceOf[MashString].s.toDouble)
    }

    override def summary = "Parse this string as a number"

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(NumberClass)

  }

  object ToPathMethod extends MashMethod("toPath") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      target.asInstanceOf[MashString].copy(tagClassOpt = Some(PathClass))
    }

    override def summary = "Tag this string as a path"

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

  }

  object LastMethod extends MashMethod("last") {

    private val N = "n"

    val params = ParameterModel(Seq(Parameter(
      name = N,
      summary = "Number of elements",
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))))

    def apply(target: Any, arguments: Arguments): MashString = {
      val boundParams = params.validate(arguments)
      val countOpt = MashNull.option(boundParams(N)).map(_.asInstanceOf[MashNumber].asInt.get)
      val s = target.asInstanceOf[MashString]
      countOpt match {
        case None    ⇒ s.last
        case Some(n) ⇒ s.modify(_.takeRight(n))
      }
    }

    override def summary = "Last character(s) of this string"

    override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

  }

  object FirstMethod extends MashMethod("first") {

    private val N = "n"

    val params = ParameterModel(Seq(Parameter(
      name = N,
      summary = "Number of elements",
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))))

    def apply(target: Any, arguments: Arguments): MashString = {
      val boundParams = params.validate(arguments)
      val countOpt = MashNull.option(boundParams(N)).map(_.asInstanceOf[MashNumber].asInt.get)
      val s = target.asInstanceOf[MashString]
      countOpt match {
        case None    ⇒ s.first
        case Some(n) ⇒ s.modify(_.take(n))
      }
    }

    override def summary = "First character(s) of the string"

    override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

  }

  object ReverseMethod extends MashMethod("reverse") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      target.asInstanceOf[MashString].reverse
    }

    override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

    override def summary = "Reverse this string"

  }

  object StartsWithMethod extends MashMethod("startsWith") {

    private val Prefix = "prefix"

    val params = ParameterModel(Seq(
      Parameter(
        Prefix,
        "Prefix to test")))

    def apply(target: Any, arguments: Arguments): MashBoolean = {
      val boundParams = params.validate(arguments)
      val s = target.asInstanceOf[MashString]
      val pattern = boundParams(Prefix).asInstanceOf[MashString]
      MashBoolean(s.startsWith(pattern))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Check if this string starts with another"

  }

  override def summary = "A string"

}

object SameStringMethodTypeInferenceStrategy extends MethodTypeInferenceStrategy {

  override def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
    targetTypeOpt.orElse(Some(Type.Instance(StringClass)))

}