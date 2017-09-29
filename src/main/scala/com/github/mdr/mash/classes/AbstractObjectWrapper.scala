package com.github.mdr.mash.classes

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.runtime._

abstract class AbstractObjectWrapper(targetValue: MashValue) {

  protected val target: MashObject = targetValue match {
    case obj: MashObject ⇒ obj
    case _               ⇒ throw EvaluatorException(s"target is of type ${targetValue.typeName}, not Object")
  }

  protected def getField(field: Field): MashValue =
    target.get(field).getOrElse(throw EvaluatorException(s"No field '${field.name}' found in object"))

  protected def getOptionalField(field: Field): Option[MashValue] =
    getField(field) match {
      case MashNull ⇒ None
      case v        ⇒ Some(v)
    }

  protected def getStringField(field: Field): String = getField(field) match {
    case s: MashString ⇒ s.s
    case v             ⇒ throwFieldTypeError(field, "String", v)
  }

  protected def getClassField(field: Field): MashClass = getField(field) match {
    case klass: MashClass ⇒ klass
    case v                ⇒ throwFieldTypeError(field, "Class", v)
  }

  protected def getOptionalStringField(field: Field): Option[String] = getOptionalField(field) map {
    case s: MashString ⇒ s.s
    case v             ⇒ throwFieldTypeError(field, "String", v)
  }

  protected def getNumberField(field: Field): Double = getField(field) match {
    case s: MashNumber ⇒ s.n
    case v             ⇒ throwFieldTypeError(field, "Number", v)
  }

  protected def getBooleanField(field: Field): Boolean = getField(field) match {
    case s: MashBoolean ⇒ s.value
    case v              ⇒ throwFieldTypeError(field, "Boolean", v)
  }

  protected def getListField(field: Field): Seq[MashValue] = getField(field) match {
    case xs: MashList ⇒ xs.elements
    case v            ⇒ throwFieldTypeError(field, "List", v)
  }

  protected def getObjectField(field: Field): MashObject = getField(field) match {
    case obj: MashObject ⇒ obj
    case v               ⇒ throwFieldTypeError(field, "Object", v)
  }

  private def throwFieldTypeError(field: Field, expectedType: String, value: MashValue) =
    throw EvaluatorException(s"Field '${field.name}' should have type $expectedType, but was '${value.typeName}'")

}
