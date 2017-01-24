package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.{ HasName, MashFunction, MashMethod, Namespace }
import com.github.mdr.mash.inference.{ Type, TypedArguments }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.MashValue

import scala.collection.immutable.ListMap

object MashClass {

  val ConstructorMethodName = "new"

  /**
    * Create an alias to a method
    */
  def alias(name: String, method: MashMethod): MashMethod = new MashMethod(name) {

    val params = method.params

    def apply(target: MashValue, arguments: Arguments): MashValue =
      method.apply(target, arguments)

    override def typeInferenceStrategy = method.typeInferenceStrategy

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
      method.getCompletionSpecs(argPos, targetTypeOpt, arguments)

    override def summary = method.summary

    override def descriptionOpt = method.descriptionOpt

  }

}

abstract class MashClass(val nameOpt: Option[String],
                         val namespaceOpt: Option[Namespace] = None) extends MashValue with HasName {

  def this(s: String) = this(s.split("\\.").lastOption, Some(Namespace(s.split("\\.").init)))

  def fields: Seq[Field] = Seq()

  def getField(fieldName: String): Option[Field] = fields.find(_.name == fieldName)

  lazy val fieldsMap: ListMap[String, Field] = {
    val pairs = for (field ← fields) yield field.name -> field
    ListMap(pairs: _*)
  }

  def methods: Seq[MashMethod] = Seq()

  def staticMethods: Seq[MashFunction] = Seq()

  def getMethod(name: String) = methods.find(_.name == name)

  lazy val memberNames: Seq[String] = fields.map(_.name) ++ methods.map(_.name)

  def getStaticMethod(name: String): Option[MashFunction] = staticMethods.find(_.name == name)

  override def toString = fullyQualifiedName.toString

  def parentOpt: Option[MashClass] = Some(ObjectClass)

  def enumerationValues: Option[Seq[String]] = None

  def summary: String

  def descriptionOpt: Option[String] = None

  def isSubClassOf(otherClass: MashClass): Boolean =
    this == otherClass || this.parentOpt.exists(_ isSubClassOf otherClass)

  def withGenerics(types: Type*): Type.Generic = Type.Generic(this, types: _*)

}

case class Field(name: String,
                 summaryOpt: Option[String] = None,
                 fieldType: Type,
                 descriptionOpt: Option[String] = None)