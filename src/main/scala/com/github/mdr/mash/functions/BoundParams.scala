package com.github.mdr.mash.functions

import java.nio.file.Path

import scala.util.control.Exception._

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.parser.AbstractSyntax.Argument
import com.github.mdr.mash.runtime._

case class BoundParams(params: Map[String, MashValue],
                       argumentNodes: Map[String, Seq[Argument]]) {

  def apply(param: String): MashValue = params(param)

  def apply(param: Parameter): MashValue = params(param.name)

  @throws[EvaluatorException]
  def throwInvalidArgument(param: Parameter, message: String): Nothing = {
    val fullMessage = s"Invalid argument '${param.name}'. $message"
    throw new ArgumentException(fullMessage, locationOpt(param))
  }

  private def mergeLocation(location1: SourceLocation, location2: SourceLocation): SourceLocation =
    SourceLocation(location1.provenance, location1.pointedRegion merge location2.pointedRegion)

  private def locationOpt(param: Parameter): Option[SourceLocation] =
    argumentNodes.get(param.name).map(nodes ⇒ nodes.flatMap(_.sourceInfoOpt).map(_.location).reduce(mergeLocation))

  def validateSequence(param: Parameter): Seq[MashValue] = this(param) match {
    case xs: MashList          ⇒ xs.items
    case MashString(s, tagOpt) ⇒ s.toSeq.map(c ⇒ MashString(c.toString, tagOpt))
    case x ⇒
      val message = s"Invalid argument '${param.name}'. Must be a sequence, but was a ${x.typeName}"
      throw new ArgumentException(message, locationOpt(param))
  }

  def validateString(param: Parameter): MashString = this(param) match {
    case s: MashString ⇒ s
    case x ⇒
      val message = s"Invalid argument '${param.name}'. Must be a String, but was a ${x.typeName}"
      throw new ArgumentException(message, locationOpt(param))
  }

  def validateObject(param: Parameter): MashObject = this(param) match {
    case obj: MashObject ⇒ obj
    case x ⇒
      val message = s"Invalid argument '${param.name}'. Must be an Object, but was a ${x.typeName}"
      throw new ArgumentException(message, locationOpt(param))
  }

  def validateStringOpt(param: Parameter): Option[MashString] = this(param) match {
    case s: MashString ⇒ Some(s)
    case MashNull      ⇒ None
    case x ⇒
      val message = s"Invalid argument '${param.name}'. Must be a string, but was a ${x.typeName}"
      throw new ArgumentException(message, locationOpt(param))
  }

  def validateFunction(param: Parameter): MashValue ⇒ MashValue =
    this(param) match {
      case f @ (_: MashString | _: MashFunction | _: BoundMethod) ⇒
        FunctionHelpers.interpretAsFunction(f)
      case x ⇒
        val message = s"Invalid argument '${param.name}'. Must be a function, but was a ${x.typeName}"
        throw new ArgumentException(message, locationOpt(param))
    }

  def validateClass(param: Parameter): MashClass =
    this(param) match {
      case klass: MashClass ⇒
        klass
      case x ⇒
        val message = s"Invalid argument '${param.name}'. Must be a class, but was a ${x.typeName}"
        throw new ArgumentException(message, locationOpt(param))
    }

  def validatePath(param: Parameter): Path = {
    val arg = this(param)
    FunctionHelpers.safeInterpretAsPath(arg) match {
      case Some(path) ⇒ path
      case None ⇒
        val message = s"Invalid argument '${param.name}'. Must be a path, but was a ${arg.typeName}"
        throw new ArgumentException(message, locationOpt(param))
    }
  }

  def validatePaths(param: Parameter): Seq[Path] = {
    val arg = this(param)
    catching(classOf[EvaluatorException]) opt FunctionHelpers.interpretAsPaths(arg) getOrElse (
      throw new ArgumentException(s"Invalid argument '${param.name}', could not interpret value of type ${arg.typeName} as path.", locationOpt(param)))
  }

  object MashInteger {

    def unapply(x: MashValue): Option[Int] = x match {
      case n: MashNumber ⇒ n.asInt
      case _             ⇒ None
    }

  }

  def validateInteger(param: Parameter): Int = validateInteger(param, this(param))

  def validateIntegerOpt(param: Parameter): Option[Int] =
    this(param) match {
      case MashNull ⇒ None
      case value    ⇒ Some(validateInteger(param, value))
    }

  private def validateInteger(param: Parameter, v: MashValue): Int = v match {
    case MashInteger(n) ⇒
      n
    case n: MashNumber ⇒
      val message = s"Invalid argument '${param.name}'. Must be an integer, but was '$n'"
      throw new ArgumentException(message, locationOpt(param))
    case x ⇒
      val message = s"Invalid argument '${param.name}'. Must be an integer, but was a ${x.typeName}"
      throw new ArgumentException(message, locationOpt(param))
  }

  def validateNumber(param: Parameter): Double = this(param) match {
    case MashNumber(n, _) ⇒
      n
    case value ⇒
      val message = s"Invalid argument '${param.name}'. Must be an integer, but was a ${value.typeName}"
      throw new ArgumentException(message, locationOpt(param))
  }

}