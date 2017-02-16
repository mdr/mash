package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.runtime._

object NumberClass extends MashClass("core.Number") {

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
    WeeksMethod)

  object ToMethod extends MashMethod("to") {

    object Params {
      val End = Parameter(
        nameOpt = Some("end"),
        summaryOpt = Some("Final number in sequence (inclusive)"))
      val Step = Parameter(
        nameOpt = Some("step"),
        summaryOpt = Some("The number to increase by for each step of the sequence (default 1)"),
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
      MashList(start.to(end, step).map(MashNumber(_)))
    }

    override def typeInferenceStrategy = Seq(NumberClass)

    override def summaryOpt = Some("Return a list of values from this number to the given end value (inclusive)")

  }

  object UntilMethod extends MashMethod("until") {

    object Params {
      val End = Parameter(
        nameOpt = Some("end"),
        summaryOpt = Some("Final number in sequence (exclusive)"))
      val Step = Parameter(
        nameOpt = Some("step"),
        summaryOpt = Some("The number to increase by for each step of the sequence (default 1)"),
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
      MashList(start.until(end, step).map(MashNumber(_)))
    }

    override def typeInferenceStrategy = Seq(NumberClass)

    override def summaryOpt = Some("Return a list of values from this number to the given end value (exclusive)")

  }

  object TimesMethod extends MashMethod("times") {

    object Params {
      val Block = Parameter(
        nameOpt = Some("block"),
        summaryOpt = Some("Code to execute"),
        isLazy = true)
    }
    import Params._

    val params = ParameterModel(Seq(Block))

    def apply(target: MashValue, arguments: Arguments): MashList = {
      val boundParams = params.validate(arguments)
      val f = boundParams(Block).asInstanceOf[MashFunction]
      val iterations = target.asInstanceOf[MashNumber].asInt.getOrElse(
        throw new EvaluatorException("Can only call this method on an integer, but was " + target))
      val results =
        for (n ← 1 to iterations)
          yield f.apply(Arguments(Seq()))
      MashList(results)
    }

    override def typeInferenceStrategy = new MethodTypeInferenceStrategy {

      def inferTypes(inferencer: Inferencer,  targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
        val argBindings = params.bindTypes(arguments)
        argBindings.getType(Params.Block).map(_.seq)
      }

    }

    override def summaryOpt = Some("Run the given argument this amount of times")

  }


  object ToIntMethod extends MashMethod("toInt") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].modify(n ⇒ if (n < 0) math.ceil(n) else math.floor(n))
    }

    override def typeInferenceStrategy = new MethodTypeInferenceStrategy {
      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
        targetTypeOpt
    }

    override def summaryOpt = Some("Convert number to an integer (rounding towards zero)")

  }

  object TagMethod extends MashMethod("tag") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].tagClassOpt.getOrElse(MashNull)
    }

    override def typeInferenceStrategy = ClassClass

    override def summaryOpt = Some("This number's tagged type if any, else null")

  }

  object UntaggedMethod extends MashMethod("untagged") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].copy(tagClassOpt = None)
    }

    override def typeInferenceStrategy = NumberClass

    override def summaryOpt = Some("This number without any tag class")
  }

  object BytesMethod extends MashMethod("bytes") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].copy(tagClassOpt = Some(BytesClass))
    }

    override def typeInferenceStrategy = NumberClass taggedWith BytesClass

    override def summaryOpt = Some("This number of bytes")
  }

  object KbMethod extends MashMethod("kb") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val n = target.asInstanceOf[MashNumber]
      MashNumber(n.n * 1024, BytesClass)
    }

    override def typeInferenceStrategy = NumberClass taggedWith BytesClass

    override def summaryOpt = Some("This number of kilobytes")
  }

  object MbMethod extends MashMethod("mb") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val n = target.asInstanceOf[MashNumber]
      MashNumber(n.n * 1024 * 1024, BytesClass)
    }

    override def typeInferenceStrategy = NumberClass taggedWith BytesClass

    override def summaryOpt = Some("This number of megabytes")
  }

  object GbMethod extends MashMethod("gb") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val n = target.asInstanceOf[MashNumber]
      MashNumber(n.n * 1024 * 1024 * 1024, BytesClass)
    }

    override def typeInferenceStrategy = NumberClass taggedWith BytesClass

    override def summaryOpt = Some("This number of gigabytes")

  }

  abstract class ChronoUnitMethod(name: String, klass: MashClass) extends MashMethod(name) {

    override def aliases = Seq(name.init)

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].withTag(klass)
    }

    override def typeInferenceStrategy = NumberClass taggedWith klass

    override def summaryOpt = Some("This number of " + name)

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

    override object typeInferenceStrategy extends MethodTypeInferenceStrategy {
      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
        targetTypeOpt
    }

    override def summaryOpt = Some("Negate this number")

  }

  override def summaryOpt = Some("A number")

  def taggedWith(klass: MashClass) = Type.Tagged(this, klass)

  override def parentOpt = Some(AnyClass)

}