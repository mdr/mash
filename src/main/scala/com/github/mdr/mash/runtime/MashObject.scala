package com.github.mdr.mash.runtime

import com.github.mdr.mash.GlobalInterpreterLock.withLock
import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.evaluator.{ EvaluatorException, ToStringifier }

import scala.PartialFunction._
import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap
import scala.language.implicitConversions
import scala.reflect.ClassTag

object MashObject {

  def of[T <% ViewableAsFields](fields: T, classOpt: Option[MashClass]): MashObject =
    new MashObject(fields.fields, classOpt)

  def of[T <% ViewableAsFields](fields: T, klass: MashClass): MashObject =
    new MashObject(fields.fields, Some(klass))

  def of[T <% ViewableAsFields](fields: T): MashObject =
    new MashObject(fields.fields, None)

  def empty = of(Seq[(MashString, MashString)]())

  def merge(objects: Seq[MashObject]) = objects.reduceOption(_ ++ _) getOrElse MashObject.empty
}

case class MashObject private(fields: LinkedHashMap[MashValue, MashValue],
                              classOpt: Option[MashClass] = None) extends MashValue with Comparable[MashObject] {
  for (klass ← classOpt) {
    val classFields = klass.fields.map(_.name).toSet
    val providedFields = fields.keySet.collect { case s: MashString ⇒ s.s }

    val missingFields = classFields diff providedFields
    if (missingFields.nonEmpty)
      throw new EvaluatorException(s"Missing fields for class '$klass': ${missingFields.map(f ⇒ s"'$f'").mkString(", ")}")
  }

  def withField(fieldName: MashValue, value: MashValue): MashObject =
    MashObject.of(fields.toSeq :+ (fieldName -> value), classOpt)

  def withClass(klass: MashClass): MashObject = MashObject.of(fields, Some(klass))

  def withoutClass: MashObject = MashObject.of(fields)

  def isEmpty: Boolean = withLock { fields.isEmpty }

  def nonEmpty: Boolean = !isEmpty

  def set(fieldName: String, value: MashValue): Unit = set(MashString(fieldName), value)

  def set(fieldName: MashValue, value: MashValue): Unit = withLock {
    fields(fieldName) = value
  }

  def apply(fieldName: String): MashValue = withLock {
    fields(MashString(fieldName))
  }

  def apply(field: Field): MashValue = apply(field.name)

  def get(fieldName: String): Option[MashValue] = get(MashString(fieldName))

  def get(fieldName: MashValue): Option[MashValue] = withLock {
    fields.get(fieldName)
  }

  def get(field: Field): Option[MashValue] = get(field.name)

  def hasField(fieldName: String): Boolean = get(fieldName).isDefined

  def hasField(fieldName: MashValue): Boolean = get(fieldName).isDefined

  def -(fieldName: MashValue): MashObject = withLock {
    MashObject.of(fields.filterKeys(_ != fieldName).toSeq)
  }

  def -(fieldName: String): MashObject = this - MashString(fieldName)

  def -(fieldNames: Seq[MashValue]): MashObject = withLock {
    var result = this
    for (fieldName ← fieldNames)
      result -= fieldName
    result
  }

  def ++(that: MashObject): MashObject = withLock {
    MashObject.of((this.fields ++ that.fields).toSeq, that.classOpt orElse this.classOpt)
  }

  def reverse: MashObject = MashObject.of(immutableFields.toSeq.reverse).copy(classOpt = classOpt)

  override def toString = renderAsString

  def renderAsString = withLock {
    ToStringifier.visit(this, "{…}") {
      val fieldString = if (fields.isEmpty) "" else fields.map { case (field, value) ⇒ s"$field: $value" }.mkString(", ")
      if (fields.isEmpty) "{}" else s"{ $fieldString }"
    }
  }

  def immutableFields: ListMap[MashValue, MashValue] = withLock {
    ListMap(fields.toSeq: _*)
  }

  def stringFields: ListMap[String, MashValue] =
    for {
      (field, value) ← immutableFields
      fieldString ← condOpt(field) { case s: MashString ⇒ s.s }
    } yield fieldString -> value

  def fieldAs[T: ClassTag](field: Field): T = withLock {
    val klass = implicitly[ClassTag[T]].runtimeClass
    val value = this (field)
    if (klass isInstance value)
      value.asInstanceOf[T]
    else
      throw new EvaluatorException("Field has unexpected type " + value.typeName)
  }

  override def equals(x: Any) = withLock {
    x match {
      case that: MashObject ⇒ this.classOpt == that.classOpt && this.fields == that.fields
      case _                ⇒ false
    }
  }

  override def hashCode = withLock {
    this.fields.hashCode
  }

  override def compareTo(that: MashObject) = MashValueOrdering.compare(this.immutableFields, that.immutableFields)

  def clear() = withLock(fields.clear())

  def size: Int = immutableFields.size

}
