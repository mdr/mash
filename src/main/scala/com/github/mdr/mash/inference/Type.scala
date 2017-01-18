package com.github.mdr.mash.inference

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.functions.{ UserDefinedFunction ⇒ UDF, _ }
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.parser.AbstractSyntax._

import scala.collection.immutable.ListMap
import scala.language.implicitConversions

sealed trait Type {

  def seq = Type.Seq(this)

}

object Type {

  val Any = Instance(AnyClass)

  /**
    * Parameterised types
    */
  case class Generic(klass: MashClass, types: Type*) extends Type

  case class Seq(t: Type) extends Type

  case class Tagged(baseClass: MashClass, tagClass: MashClass) extends Type

  case class Instance(klass: MashClass) extends Type {
    override def toString = klass.toString
  }


  case class UserClass(name: String, params: ParameterModel, methods: ListMap[String, Type.UserDefinedFunction]) extends Type {
    override def toString = s"${classOf[UserClass].getSimpleName}($name)"
  }

  case class UserClassInstance(userClass: UserClass) extends Type {
    override def toString = s"${classOf[UserClassInstance].getSimpleName}(${userClass.name})"
  }

  case class Object(knownFields: Map[String, Type]) extends Type

  /**
    * Method defined in mash, bound to a target
    */
  case class BoundUserDefinedMethod(target: Type, function: UserDefinedFunction) extends Type

  /**
    * Built-in (Scala) method
    */
  case class BoundBuiltinMethod(target: Type, method: MashMethod) extends Type {
    require(!method.isInstanceOf[UserDefinedMethod])
  }

  /**
    * Function defined in Mash
    */
  case class UserDefinedFunction(nameOpt: Option[String],
                                 params: ParameterModel,
                                 body: Expr,
                                 bindings: Map[String, Type]) extends Type {
    override def toString = s"${classOf[UserDefinedFunction].getSimpleName}(${params.params.map(_.nameOpt).mkString(", ")}, $body)"
  }

  /**
    * Built-in (Scala) function
    */
  case class BuiltinFunction(f: MashFunction) extends Type {
    require(!f.isInstanceOf[UDF])
  }

  // Various implicits to make it less wordy to describe types:

  implicit def classToType[T](x: MashClass): Type = Type.Instance(x)

  implicit def seqToType[T <: MashClass](xs: scala.collection.Seq[T]): Type = {
    require(xs.length == 1)
    xs.head.seq
  }

  implicit def pathClassToType[T](x: PathClass.type): Type = StringClass taggedWith PathClass

  implicit def unitToType[T](x: Unit.type): Type = Type.Instance(UnitClass)

}
