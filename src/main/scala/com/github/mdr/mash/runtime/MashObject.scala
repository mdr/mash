package com.github.mdr.mash.runtime

import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashClass

object MashObject {

  def apply(fields: ListMap[Field, MashValue], klass: MashClass): MashObject = {
    val newFields: ListMap[String, MashValue] = for ((k, v) ← fields) yield k.name -> v
    MashObject(LinkedHashMap(newFields.toSeq: _*), Some(klass))
  }

  def apply(fields: ListMap[String, MashValue], classOpt: Option[MashClass]): MashObject =
    MashObject(LinkedHashMap(fields.toSeq: _*), classOpt)

  def apply(fields: Seq[(String, MashValue)], classOpt: Option[MashClass]): MashObject =
    MashObject(LinkedHashMap(fields: _*), classOpt)

}

case class MashObject(fields: LinkedHashMap[String, MashValue], classOpt: Option[MashClass] = None) extends MashValue {

  fields.values.foreach(MashValue.checkIsValidRuntimeValue)

  for (klass ← classOpt) {
    val klassFields = klass.fields.map(_.name)
    val providedFields = fields.keys.toSeq
    if (klassFields != providedFields)
      throw new EvaluatorException(s"Invalid fields for class $klass. Expected ${klassFields.mkString(",")}, but was ${providedFields.mkString(",")}")
  }

  def set(fieldName: String, value: MashValue) { fields(fieldName) = value }

  def apply(field: Field): MashValue = fields(field.name)

  def getField(fieldName: String): Option[MashValue] = fields.get(fieldName)

  def getField(field: Field): Option[MashValue] = getField(field.name)

  def field(field: Field): MashValue = fields(field.name)

  override def toString = {
    val fieldString = fields.map { case (k, v) ⇒ s"$k: $v" }.mkString(", ")
    val classString = classOpt.map(c ⇒ s"$c | ").getOrElse("")
    s"{ $classString$fieldString }"
  }

  def immutableFields: ListMap[String, MashValue] = ListMap(fields.toSeq: _*)

}