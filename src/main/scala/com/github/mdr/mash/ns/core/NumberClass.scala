package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.MashFunction

object NumberClass extends MashClass("core.Number") {

  import MashClass.alias

  override val methods = Seq(
    BytesMethod,
    DaysMethod,
    GbMethod,
    HoursMethod,
    KbMethod,
    MbMethod,
    MillisecondsMethod,
    MinutesMethod,
    MonthsMethod,
    NegateMethod,
    SecondsMethod,
    TagMethod,
    TimesMethod,
    ToIntMethod,
    ToMethod,
    UntaggedMethod,
    UntilMethod,
    WeeksMethod,
    alias("day", DaysMethod),
    alias("hour", HoursMethod),
    alias("millisecond", MillisecondsMethod),
    alias("minute", MinutesMethod),
    alias("month", MonthsMethod),
    alias("second", SecondsMethod),
    alias("week", WeeksMethod))

  object ToMethod extends MashMethod("to") {

    object Params {
      val End = Parameter(
        name = "end",
        summary = "Final number in sequence (inclusive)")
      val Step = Parameter(
        name = "step",
        summary = "The number to increase by for each step of the sequence (default 1)",
        defaultValueGeneratorOpt = Some(() ⇒ MashNumber(1)))
    }
    import Params._

    val params = ParameterModel(Seq(End, Step))

    def apply(target: MashValue, arguments: Arguments): MashList = {
      val boundParams = params.validate(arguments)
      val start = target.asInstanceOf[MashNumber].asInt.getOrElse(
        throw new EvaluatorException("Can only call this method on an integer, but was " + target))
      val end = boundParams.validateInteger(End)
      val step = boundParams.validateInteger(Step)
      MashList((start.to(end, step)).map(MashNumber(_)))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Seq(NumberClass))

    override def summary = "Return a list of values from this number to the given end value (inclusive)"

  }

  object UntilMethod extends MashMethod("until") {

    object Params {
      val End = Parameter(
        name = "end",
        summary = "Final number in sequence (exclusive)")
      val Step = Parameter(
        name = "step",
        summary = "The number to increase by for each step of the sequence (default 1)",
        defaultValueGeneratorOpt = Some(() ⇒ MashNumber(1)))
    }
    import Params._

    val params = ParameterModel(Seq(End, Step))

    def apply(target: MashValue, arguments: Arguments): MashList = {
      val boundParams = params.validate(arguments)
      val start = target.asInstanceOf[MashNumber].asInt.getOrElse(
        throw new EvaluatorException("Can only call this method on an integer, but was " + target))
      val end = boundParams.validateInteger(End)
      val step = boundParams.validateInteger(Step)
      MashList((start.until(end, step)).map(MashNumber(_)))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Seq(NumberClass))

    override def summary = "Return a list of values from this number to the given end value (exclusive)"

  }

  object TimesMethod extends MashMethod("times") {

    object Params {
      val Block = Parameter(
        name = "block",
        summary = "Code to execute",
        isLazy = true)
    }
    import Params._

    val params = ParameterModel(Seq(Block))

    def apply(target: MashValue, arguments: Arguments): MashUnit = {
      val boundParams = params.validate(arguments)
      val f = boundParams(Block).asInstanceOf[MashFunction]
      val iterations = target.asInstanceOf[MashNumber].asInt.getOrElse(
        throw new EvaluatorException("Can only call this method on an integer, but was " + target))
      for (n ← 1 to iterations)
        f.apply(Arguments(Seq()))
      MashUnit
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summary = "Run the given argument this amount of times"

  }

  object ToIntMethod extends MashMethod("toInt") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].modify(_.toInt)
    }

    override def typeInferenceStrategy = new MethodTypeInferenceStrategy {
      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
        targetTypeOpt
    }

    override def summary = "Convert number to an integer (rounding towards zero)"

  }

  object TagMethod extends MashMethod("tag") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].tagClassOpt.getOrElse(MashNull)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(ClassClass)

    override def summary = "This number's tagged type if any, else null"

  }

  object UntaggedMethod extends MashMethod("untagged") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].copy(tagClassOpt = None)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(NumberClass)

    override def summary = "This number without any tag class"
  }

  object BytesMethod extends MashMethod("bytes") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].copy(tagClassOpt = Some(BytesClass))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(NumberClass, BytesClass))

    override def summary = "This number of bytes"
  }

  object KbMethod extends MashMethod("kb") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val n = target.asInstanceOf[MashNumber]
      MashNumber(n.n * 1024, BytesClass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(NumberClass, BytesClass))

    override def summary = "This number of kilobytes"
  }

  object MbMethod extends MashMethod("mb") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val n = target.asInstanceOf[MashNumber]
      MashNumber(n.n * 1024 * 1024, BytesClass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(NumberClass, BytesClass))

    override def summary = "This number of megabytes"
  }

  object GbMethod extends MashMethod("gb") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val n = target.asInstanceOf[MashNumber]
      MashNumber(n.n * 1024 * 1024 * 1024, BytesClass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(NumberClass, BytesClass))

    override def summary = "This number of gigabytes"

  }

  abstract class ChronoUnitMethod(name: String, klass: MashClass) extends MashMethod(name) {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].withTag(klass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(NumberClass, klass))

    override def summary = "This number of " + name

  }

  object MillisecondsMethod extends ChronoUnitMethod("milliseconds", MillisecondsClass)
  object SecondsMethod extends ChronoUnitMethod("seconds", SecondsClass)
  object MinutesMethod extends ChronoUnitMethod("minutes", MinutesClass)
  object HoursMethod extends ChronoUnitMethod("hours", HoursClass)
  object DaysMethod extends ChronoUnitMethod("days", DaysClass)
  object WeeksMethod extends ChronoUnitMethod("weeks", WeeksClass)
  object MonthsMethod extends ChronoUnitMethod("months", MonthsClass)

  object NegateMethod extends MashMethod("negate") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val n = target.asInstanceOf[MashNumber]
      n.negate
    }

    override def typeInferenceStrategy = new MethodTypeInferenceStrategy {
      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
        targetTypeOpt
    }

    override def summary = "Negate this number"

  }

  def summary = "A number"

  def taggedWith(klass: MashClass) = Type.Tagged(this, klass)

  override def parentOpt = Some(AnyClass)

}