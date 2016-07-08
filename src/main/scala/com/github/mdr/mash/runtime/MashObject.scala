package com.github.mdr.mash.runtime

import scala.language.implicitConversions
import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.ToStringifier

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
      throw new EvaluatorException(s"Invalid fields for class $klass. Expected ${klassFields.mkString(",")}, but was ${providedFields.mkString(",")}")
  }

  def set(fieldName: String, value: MashValue) { fields(fieldName) = value }

  def apply(fieldName: String): MashValue = fields(fieldName)

  def apply(field: Field): MashValue = fields(field.name)

  def get(fieldName: String): Option[MashValue] = fields.get(fieldName)

  def get(field: Field): Option[MashValue] = fields.get(field.name)

  def -(fieldName: String): MashObject = {
    val newFields = LinkedHashMap(fields.filterKeys(_ != fieldName).toSeq: _*)
    MashObject(newFields)
  }

  def +(that: MashObject): MashObject = {
    val newFields = LinkedHashMap((this.fields ++ that.fields).toSeq: _*)
    MashObject(newFields, classOpt = None)
  }

  override def toString = asString

  def asString = ToStringifier.visit(this, "{…}") {
    val fieldString = fields.map { case (k, v) ⇒ s"$k: $v" }.mkString(", ")
    val classString = classOpt.map(c ⇒ s"$c | ").getOrElse("")
    s"{ $classString$fieldString }"
  }

  def immutableFields: ListMap[String, MashValue] = ListMap(fields.toSeq: _*)

}