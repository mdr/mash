package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.ns.collections.ListClass
import java.time.Instant
import com.github.mdr.mash.functions.MashFunction
import java.time.LocalDate
import scala.collection.immutable.ListMap
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.runtime.MashUnit
import com.github.mdr.mash.runtime.MashWrapped
import com.github.mdr.mash.runtime.MashValue

object ObjectClass extends MashClass("core.Object") {

  override val methods = Seq(
    ClassMethod,
    FieldsMethod,
    ToStringMethod,
    InMethod)

  object ClassMethod extends MashMethod("class") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashClass = target.primaryClass
    
    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(ClassClass)

    override def summary = "The class of this object"

  }

  object FieldsMethod extends MashMethod("fields") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = target match {
      case MashObject(fields, _) ⇒
        MashList(fields.toSeq.map {
          case (name, value) ⇒
            import FieldAndValueClass.Fields._
            MashObject(ListMap(
              Name -> MashString(name),
              Value -> value), FieldAndValueClass)
        })
      case _ ⇒
        MashList(Seq())
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Seq(FieldAndValueClass))

    override def summary = "Return the fields of this object"
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

  override def summary = "Then root class of all objects"

}