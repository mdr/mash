package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel

object NumberClass extends MashClass("core.Number") {

  import MashClass.alias

  override val methods = Seq(
    BytesMethod,
    DaysMethod,
    GbMethod,
    HoursMethod,
    KbMethod,
    MbMethod,
    MonthsMethod,
    NegateMethod,
    TagMethod,
    ToIntMethod,
    UntaggedMethod,
    WeeksMethod,
    alias("day", DaysMethod),
    alias("hour", HoursMethod),
    alias("month", MonthsMethod),
    alias("week", WeeksMethod))

  object ToIntMethod extends MashMethod("toInt") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].modify(n ⇒ n.toInt)
    }

    override def typeInferenceStrategy = new MethodTypeInferenceStrategy {
      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
        targetTypeOpt
    }

    override def summary = "Convert number to an integer (rounding towards zero)"

  }
  
  object TagMethod extends MashMethod("tag") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashClass = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].tagClassOpt.orNull
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(ClassClass))

    override def summary = "This number's tagged type if any, else null"

  }

  object UntaggedMethod extends MashMethod("untagged") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].copy(tagClassOpt = None)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(NumberClass))

    override def summary = "This number without any tag class"
  }

  object BytesMethod extends MashMethod("bytes") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].copy(tagClassOpt = Some(BytesClass))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(NumberClass, BytesClass))

    override def summary = "This number of bytes"
  }

  object KbMethod extends MashMethod("kb") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val n = target.asInstanceOf[MashNumber]
      MashNumber(n.n * 1024, BytesClass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(NumberClass, BytesClass))

    override def summary = "This number of kilobytes"
  }

  object MbMethod extends MashMethod("mb") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val n = target.asInstanceOf[MashNumber]
      MashNumber(n.n * 1024 * 1024, BytesClass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(NumberClass, BytesClass))

    override def summary = "This number of megabytes"
  }

  object GbMethod extends MashMethod("gb") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val n = target.asInstanceOf[MashNumber]
      MashNumber(n.n * 1024 * 1024 * 1024, BytesClass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(NumberClass, BytesClass))

    override def summary = "This number of gigabytes"

  }

  abstract class ChronoUnitMethod(name: String, klass: MashClass) extends MashMethod(name) {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      target.asInstanceOf[MashNumber].withTag(klass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(NumberClass, klass))

    override def summary = "This number of " + name

  }

  object DaysMethod extends ChronoUnitMethod("days", DaysClass)
  object HoursMethod extends ChronoUnitMethod("hours", HoursClass)
  object WeeksMethod extends ChronoUnitMethod("weeks", WeeksClass)
  object MonthsMethod extends ChronoUnitMethod("months", MonthsClass)

  object NegateMethod extends MashMethod("negate") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashNumber = {
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
}