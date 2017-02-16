package com.github.mdr.mash.classes

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.runtime._

abstract class AbstractObjectWrapper(targetValue: MashValue) {

  protected val target = targetValue match {
    case obj: MashObject ⇒ obj
    case _               ⇒ throw new EvaluatorException(s"target is of type ${targetValue.typeName}, not Object")
  }

  protected def getField(field: Field): MashValue =
    target.get(field).getOrElse(throw new EvaluatorException(s"No field '${field.name}' found in object"))

  protected def getStringField(field: Field): String = getField(field) match {
    case s: MashString ⇒ s.s
    case v             ⇒ throw new EvaluatorException(s"Field '${field.name}' should have type String, but was '${v.typeName}'")
  }

  protected def getOptionalStringField(field: Field): Option[String] = getField(field) match {
    case MashNull      ⇒ None
    case s: MashString ⇒ Some(s.s)
    case v             ⇒ throw new EvaluatorException(s"Field '${field.name}' should have type String, but was '${v.typeName}'")
  }

  protected def getNumberField(field: Field): Double = getField(field) match {
    case s: MashNumber ⇒ s.n
    case v             ⇒ throw new EvaluatorException(s"Field '${field.name}' should have type Number, but was '${v.typeName}'")
  }

  protected def getListField(field: Field): Seq[MashValue] = getField(field) match {
    case xs: MashList ⇒ xs.elements
    case v            ⇒ throw new EvaluatorException(s"Field '${field.name}' should have type List, but was '${v.typeName}'")
  }

}
