package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.ns.collections.SeqClass
import java.time.Instant
import com.github.mdr.mash.functions.MashFunction
import java.time.LocalDate
import scala.collection.immutable.ListMap

object ObjectClass extends MashClass("core.Object") {

  override val methods = Seq(
    ClassMethod,
    FieldsMethod,
    ToStringMethod,
    InMethod)

  object ClassMethod extends MashMethod("class") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashClass = target match {
      case null            ⇒ NullClass
      case ()              ⇒ UnitClass
      case obj: MashObject ⇒ obj.classOpt.getOrElse(ObjectClass)
      case _: MashNumber   ⇒ NumberClass
      case _: MashString   ⇒ StringClass
      case _: Boolean      ⇒ BooleanClass
      case _: Seq[_]       ⇒ SeqClass
      case _: Instant      ⇒ DateTimeClass
      case _: MashFunction ⇒ FunctionClass
      case _: BoundMethod  ⇒ BoundMethodClass
      case _: MashClass    ⇒ ClassClass
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(ClassClass)

    override def summary = "The class of the object"

  }

  object FieldsMethod extends MashMethod("fields") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashList = target match {
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

    def apply(target: Any, arguments: Arguments): MashString = MashString(stringify(target))

    def stringify(x: Any): String = x match {
      case MashString(s, _) ⇒ s
      case klass: MashClass ⇒ klass.fullyQualifiedName.toString
      case xs: MashList     ⇒ xs.items.map(ToStringifier.stringify).mkString("[", ", ", "]")
      case ()               ⇒ ""
      case _                ⇒ "" + x
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

    def apply(target: Any, arguments: Arguments): Boolean = {
      val boundParams = params.validate(arguments)
      val sequence = boundParams.validateSequence(Sequence)
      sequence.contains(target)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Check whether this element is contained in a sequence"
  }

  override def parentOpt: Option[MashClass] = None

  override def summary = "Then root class of all objects"

}