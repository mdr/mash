package com.github.mdr.mash.ns.core

import java.time.{ Instant, LocalDate }

import com.github.mdr.mash.evaluator.{ Arguments, BoundMethod, MashClass }
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.utils.NumberUtils

object AnyClass extends MashClass("core.Any") {

  override val methods = Seq(
    ClassMethod,
    InMethod,
    IsAMethod,
    ToStringMethod)

  object IsAMethod extends MashMethod("isA") {

    object Params {
      val Class = Parameter(
        name = "class",
        summary = "Class to check")
    }
    import Params._

    val params = ParameterModel(Seq(Class))

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      val boundParams = params.validate(arguments)
      val klass = boundParams.validateClass(Class)
      MashBoolean(target isA klass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Return true if and only if this value is an instance of the given class"

    override def descriptionOpt = Some("""Examples:
  42.isA Number  # true
  42.isA Boolean # false
  42.isA Any     # true
""")

  }

  object ClassMethod extends MashMethod("class") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashClass = target.primaryClass

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(ClassClass)

    override def summary = "The class of this object"

  }

  object ToStringMethod extends MashMethod("toString") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = target match {
      case s: MashString ⇒ s
      case _             ⇒ MashString(stringify(target))
    }

    private def stringify(value: MashValue): String = value match {
      case MashNull                  ⇒ "null"
      case MashUnit                  ⇒ ""
      case obj: MashObject           ⇒ obj.asString
      case MashNumber(n, _)          ⇒ NumberUtils.prettyString(n)
      case MashBoolean(b)            ⇒ b.toString
      case MashWrapped(i: Instant)   ⇒ i.toString
      case MashWrapped(d: LocalDate) ⇒ d.toString
      case f: MashFunction           ⇒ f.fullyQualifiedName.toString
      case method: BoundMethod       ⇒ method.fullyQualifiedName
      case klass: MashClass          ⇒ klass.fullyQualifiedName.toString
      case xs: MashList              ⇒ xs.asString
      case _                         ⇒ "???"
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

    override def summary = "Represent this object as a string"

    override def descriptionOpt = Some("""Examples:
  42.toString   # "42"
  null.toString # "null"
""")

  }

  object InMethod extends MashMethod("in") {

    object Params {
      val Sequence = Parameter(
        "sequence",
        "Sequence to check if element is contained in")
    }
    import Params._

    val params = ParameterModel(Seq(Sequence))

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      val boundParams = params.validate(arguments)
      val sequence = boundParams.validateSequence(Sequence)
      MashBoolean(sequence.contains(target))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Check whether this element is contained in a sequence"

    override def descriptionOpt = Some("""Examples:
  2.in [1, 2, 3] # true
  4.in [1, 2, 3] # false
  "i".in "team"  # false
""")

  }

  override def parentOpt: Option[MashClass] = None

  override def summary = "The root class of all Mash values"

}