package com.github.mdr.mash.runtime

import com.github.mdr.mash.evaluator.{ EvaluatorException, Field, MashClass, ToStringifier }

import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap
import scala.language.implicitConversions
import scala.reflect.ClassTag

trait ViewableAsFields {
  def fields: LinkedHashMap[String, MashValue]
}

object ViewableAsFields {

  implicit def fromLinkedHashMap(map: LinkedHashMap[String, MashValue]): ViewableAsFields = new ViewableAsFields { def fields = map }
  implicit def fromMap(map: Map[String, MashValue]): ViewableAsFields = new ViewableAsFields { def fields = LinkedHashMap(map.toSeq: _*) }
  implicit def fromListMap(map: Map[Field, MashValue]): ViewableAsFields = new ViewableAsFields {
    def fields = LinkedHashMap(map.toSeq.map { case (field, v) ⇒ field.name -> v }: _*)
  }
  implicit def fromPairs(pairs: Seq[(String, MashValue)]): ViewableAsFields = new ViewableAsFields { def fields = LinkedHashMap(pairs: _*) }

}

object MashObject {

  def of[T <% ViewableAsFields](fields: T, classOpt: Option[MashClass]): MashObject =
    MashObject(fields.fields, classOpt)

  def of[T <% ViewableAsFields](fields: T, klass: MashClass): MashObject =
    MashObject(fields.fields, Some(klass))

  def of[T <% ViewableAsFields](fields: T): MashObject =
    MashObject(fields.fields, None)

  def empty() = of(Seq())

}

case class MashObject private (fields: LinkedHashMap[String, MashValue], classOpt: Option[MashClass] = None) extends MashValue {

  for (klass ← classOpt) {
    val klassFields = klass.fields.map(_.name)
    val providedFields = fields.keys.toSeq
    if (klassFields != providedFields)
      throw new EvaluatorException(s"Invalid fields for class $klass. Expected ${klassFields.mkString("[", ", ", "]")}, but was ${providedFields.mkString("[", ", ", "]")}")
  }

  def withField(fieldName: String, value: MashValue): MashObject =
    MashObject.of(fields.toSeq :+ (fieldName -> value), classOpt)

  def set(fieldName: String, value: MashValue) { fields(fieldName) = value }

  def apply(fieldName: String): MashValue = fields(fieldName)

  def apply(field: Field): MashValue = fields(field.name)

  def get(fieldName: String): Option[MashValue] = fields.get(fieldName)

  def get(field: Field): Option[MashValue] = fields.get(field.name)

  def -(fieldName: String): MashObject =
    MashObject.of(fields.filterKeys(_ != fieldName).toSeq)

  def +(that: MashObject): MashObject =
    MashObject.of((this.fields ++ that.fields).toSeq, this.classOpt)

  override def toString = asString

  def asString = ToStringifier.visit(this, "{…}") {
    val fieldString = fields.map { case (field, value) ⇒ s"$field: $value" }.mkString(", ")
    val classString = classOpt.map(c ⇒ s"$c | ").getOrElse("")
    s"{ $classString$fieldString }"
  }

  def immutableFields: ListMap[String, MashValue] = ListMap(fields.toSeq: _*)

  def fieldAs[T: ClassTag](field: Field): T = {
    val klass = implicitly[ClassTag[T]].runtimeClass
    val value = this(field)
    if (klass isInstance value)
      value.asInstanceOf[T]
    else
      throw new EvaluatorException("Field has unexpected type " + value.typeName)
  }
}