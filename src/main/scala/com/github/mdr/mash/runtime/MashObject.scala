package com.github.mdr.mash.runtime

import com.github.mdr.mash.GlobalInterpreterLock.withLock
import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.evaluator.{ EvaluatorException, ToStringifier }

import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap
import scala.language.implicitConversions
import scala.reflect.ClassTag

trait ViewableAsFields {
  def fields: LinkedHashMap[String, MashValue]
}

object ViewableAsFields {

  implicit def fromLinkedHashMap(map: LinkedHashMap[String, MashValue]): ViewableAsFields =
    new ViewableAsFields {
      def fields = map
    }

  implicit def fromMap(map: Map[String, MashValue]): ViewableAsFields =
    new ViewableAsFields {
      def fields = LinkedHashMap(map.toSeq: _*)
    }

  implicit def fromListMap(map: Map[Field, MashValue]): ViewableAsFields =
    new ViewableAsFields {
      def fields = LinkedHashMap(map.toSeq.map { case (field, v) ⇒ field.name -> v }: _*)
    }

  implicit def fromPairs(pairs: Seq[(String, MashValue)]): ViewableAsFields =
    new ViewableAsFields {
      def fields = LinkedHashMap(pairs: _*)
    }

  implicit def fromPairs(pair: (String, MashValue)): ViewableAsFields =
    new ViewableAsFields {
      def fields = LinkedHashMap(pair)
    }

}

object MashObject {

  def of[T <% ViewableAsFields](fields: T, classOpt: Option[MashClass]): MashObject =
    new MashObject(fields.fields, classOpt)

  def of[T <% ViewableAsFields](fields: T, klass: MashClass): MashObject =
    new MashObject(fields.fields, Some(klass))

  def of[T <% ViewableAsFields](fields: T): MashObject =
    new MashObject(fields.fields, None)

  def empty = of(Seq())

}

case class MashObject private(fields: LinkedHashMap[String, MashValue],
                              classOpt: Option[MashClass] = None) extends MashValue with Comparable[MashObject] {

  for (klass ← classOpt) {
    val classFields = klass.fields.map(_.name).toSet
    val providedFields = fields.keySet

    val missingFields = classFields diff providedFields
    if (missingFields.nonEmpty)
      throw new EvaluatorException(s"Missing fields for class '$klass': ${missingFields.map(f ⇒ s"'$f'").mkString(", ")}")
  }

  def withField(fieldName: String, value: MashValue): MashObject =
    MashObject.of(fields.toSeq :+ (fieldName -> value), classOpt)

  def withClass(klass: MashClass): MashObject = MashObject.of(fields, Some(klass))

  def withoutClass: MashObject = MashObject.of(fields)

  def isEmpty: Boolean = withLock { fields.isEmpty }

  def set(fieldName: String, value: MashValue) = withLock {
    fields(fieldName) = value
  }

  def apply(fieldName: String): MashValue = withLock {
    fields(fieldName)
  }

  def apply(field: Field): MashValue = withLock {
    fields(field.name)
  }

  def get(fieldName: String): Option[MashValue] = withLock {
    fields.get(fieldName)
  }

  def get(field: Field): Option[MashValue] = withLock {
    fields.get(field.name)
  }

  def hasField(fieldName: String): Boolean = get(fieldName).isDefined

  def -(fieldName: String): MashObject = withLock {
    MashObject.of(fields.filterKeys(_ != fieldName).toSeq)
  }

  def -(fieldNames: Seq[String]): MashObject = withLock {
    var result = this
    for (fieldName ← fieldNames)
      result -= fieldName
    result
  }

  def +(that: MashObject): MashObject = withLock {
    MashObject.of((this.fields ++ that.fields).toSeq, that.classOpt orElse this.classOpt)
  }

  def reverse: MashObject = MashObject.of(immutableFields.toSeq.reverse).copy(classOpt = classOpt)

  override def toString = asString

  def asString = withLock {
    ToStringifier.visit(this, "{…}") {
      val fieldString = if (fields.isEmpty) "" else fields.map { case (field, value) ⇒ s"$field: $value" }.mkString(", ")
      if (fields.isEmpty) "{}" else s"{ $fieldString }"
    }
  }

  def immutableFields: ListMap[String, MashValue] = withLock {
    ListMap(fields.toSeq: _*)
  }

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
}
