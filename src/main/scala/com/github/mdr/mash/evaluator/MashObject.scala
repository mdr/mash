package com.github.mdr.mash.evaluator

import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap

object MashObject {

  def apply(fields: ListMap[Field, Any], klass: MashClass): MashObject = {
    val newFields: ListMap[String, Any] = for ((k, v) ← fields) yield k.name -> v
    MashObject(LinkedHashMap(newFields.toSeq: _*), Some(klass))
  }

  def apply(fields: ListMap[String, Any], classOpt: Option[MashClass]): MashObject =
    MashObject(LinkedHashMap(fields.toSeq: _*), classOpt)

}

case class MashObject(fields: LinkedHashMap[String, Any], classOpt: Option[MashClass] = None) {

  fields.values.foreach(Evaluator.checkIsValidRuntimeValue)

  for (klass ← classOpt) {
    val klassFields = klass.fields.map(_.name)
    val providedFields = fields.keys.toSeq
    if (klassFields != providedFields)
      throw new EvaluatorException(s"Invalid fields for class $klass. Expected ${klassFields.mkString(",")}, but was ${providedFields.mkString(",")}")
  }

  def apply(field: Field): Any = fields(field.name)

  def getField(fieldName: String): Option[Any] = fields.get(fieldName)

  def getField(field: Field): Option[Any] = getField(field.name)

  def field(field: Field): Any = fields(field.name)

  override def toString = {
    val fieldString = fields.map { case (k, v) ⇒ s"$k: $v" }.mkString(", ")
    val classString = classOpt.map(c ⇒ s"$c | ").getOrElse("")
    s"{ $classString$fieldString }"
  }

  def immutableFields: ListMap[String, Any] = ListMap(fields.toSeq: _*)

}