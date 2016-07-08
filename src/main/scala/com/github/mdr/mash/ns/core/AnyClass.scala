package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.utils.NumberUtils
import java.time.Instant
import java.time.LocalDate
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.evaluator.BoundMethod

object AnyClass extends MashClass("core.Any") {

  override val methods = Seq(
    ClassMethod,
    IsAMethod,
    ToStringMethod,
    InMethod)

  object IsAMethod extends MashMethod("isA") {

    object Params {
      val Class = Parameter(
        name = "class",
        summary = "Class")
    }
    import Params._

    val params = ParameterModel(Seq(Class))

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      val boundParams = params.validate(arguments)
      val klass = boundParams.validateClass(Class)
      MashBoolean(target isA klass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Return true if this object is an instance of the given class"
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

    private def stringify(x: MashValue): String = x match {
      case MashNull                  ⇒ "null"
      case MashUnit                  ⇒ ""
      case obj: MashObject           ⇒ obj.asString
      case n: MashNumber             ⇒ NumberUtils.prettyString(n.n)
      case b: MashBoolean            ⇒ b.value.toString
      case MashWrapped(i: Instant)   ⇒ i.toString
      case MashWrapped(d: LocalDate) ⇒ d.toString
      case f: MashFunction           ⇒ f.fullyQualifiedName.toString
      case bm: BoundMethod           ⇒ bm.fullyQualifiedName
      case klass: MashClass          ⇒ klass.fullyQualifiedName.toString
      case xs: MashList              ⇒ xs.asString
      case _                         ⇒ "???"
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

    override def summary = "Represent this object as a string"

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
  }

  override def parentOpt: Option[MashClass] = None

  override def summary = "The root class of all Mash values"

}