package com.github.mdr.mash.classes

import com.github.mdr.mash.functions.{ HasName, MashFunction, MashMethod, Namespace }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.{ MashClassOrdering, MashValue }

import scala.collection.immutable.ListMap

object MashClass {

  val ConstructorMethodName = "new"

}

abstract class MashClass(val nameOpt: Option[String],
                         val namespaceOpt: Option[Namespace] = None) extends MashValue with HasName with Comparable[MashClass]  {

  def this(s: String) = this(s.split("\\.").lastOption, Some(Namespace(s.split("\\.").init)))

  def fields: Seq[Field] = Seq()

  lazy val fieldsMap: ListMap[String, Field] = {
    val pairs = for (field ← fields) yield field.name -> field
    ListMap(pairs: _*)
  }

  def methods: Seq[MashMethod] = Seq()

  def staticMethods: Seq[MashFunction] = Seq()

  def getField(fieldName: String): Option[Field] = fields.find(_.name == fieldName)

  def getMethod(name: String) = methods.find(method ⇒ method.name == name || method.aliases.contains(name))

  def getStaticMethod(name: String): Option[MashFunction] = staticMethods.find(_.name == name)

  def methodNames = methods.flatMap(_.names)

  lazy val memberNames: Seq[String] = (fields.map(_.name) ++ methods.flatMap(_.names)).distinct

  override def toString = fullyQualifiedName.toString

  def parentOpt: Option[MashClass] = Some(ObjectClass)

  def enumerationValues: Option[Seq[String]] = None

  def summaryOpt: Option[String]

  def descriptionOpt: Option[String] = None

  def isSubClassOf(otherClass: MashClass): Boolean =
    this == otherClass || this.parentOpt.exists(_ isSubClassOf otherClass)

  def withGenerics(types: Type*): Type.Generic = Type.Generic(this, types: _*)

  def compareTo(that: MashClass) = MashClassOrdering.compare(this, that)


}
