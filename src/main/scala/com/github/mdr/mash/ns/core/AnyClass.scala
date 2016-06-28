package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.runtime._

object AnyClass extends MashClass("core.Any") {

  override val methods = Seq(
    ClassMethod,
    ToStringMethod,
    InMethod)

  object ClassMethod extends MashMethod("class") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashClass = target.primaryClass
    
    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(ClassClass)

    override def summary = "The class of this object"

  }

  object ToStringMethod extends MashMethod("toString") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments) = MashString(stringify(target))

    def stringify(x: MashValue): String = x match {
      case MashString(s, _) ⇒ s
      case klass: MashClass ⇒ klass.fullyQualifiedName.toString
      case xs: MashList     ⇒ xs.items.map(ToStringifier.stringify).mkString("[", ", ", "]")
      case MashUnit         ⇒ ""
      case _                ⇒ x.toString
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