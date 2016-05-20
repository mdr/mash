package com.github.mdr.mash.inference

import scala.language.implicitConversions
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.collections._
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.parser.AbstractSyntax._
import scala.collection.immutable.ListMap
import com.github.mdr.mash.ns.os.PathClass

sealed trait Type

trait TypeFunction {

  def apply(positionArgs: Seq[Option[Type]], argSet: Set[String], argValues: Map[String, Type]): Option[Type]

}

object Type {

  case object Any extends Type
  case class Seq(t: Type) extends Type
  case class Group(keyType: Type, valuesType: Type) extends Type
  case class Tagged(baseClass: MashClass, tagClass: MashClass) extends Type
  case class Instance(klass: MashClass) extends Type { override def toString = klass.toString }
  case class Object(knownFields: ListMap[String, Type]) extends Type
  case class DefinedFunction(f: MashFunction) extends Type
  case class BoundMethod(receiver: Type, method: MashMethod) extends Type
  case class Lambda(parameter: String, body: Expr, bindings: Map[String, Type]) extends Type {
    override def toString = s"Lambda($parameter, $body)"
  }

  // Various implicits to make it less wordy to describe types
  implicit def classToType[T](x: MashClass): Type = Type.Instance(x)
  implicit def seqToType[T <: MashClass](xs: scala.collection.Seq[T]): Type = {
    require(xs.length == 1)
    Type.Seq(xs.head)
  }
  implicit def pathClassToType[T](x: PathClass.type): Type = Type.Tagged(StringClass, PathClass)
  implicit def unitToType[T](x: Unit.type): Type = Type.Instance(UnitClass)
}
