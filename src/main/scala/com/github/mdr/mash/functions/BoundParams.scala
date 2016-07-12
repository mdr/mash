package com.github.mdr.mash.functions

import java.nio.file.Path

import scala.util.control.Exception._

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.parser.AbstractSyntax.Argument
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.utils.PointedRegion

case class BoundParams(params: Map[String, MashValue], argumentNodes: Map[String, Seq[Argument]]) {

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
      val message = s"Invalid argument '${param.name}'. Must be a string, but was a ${x.typeName}"
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
        (o ⇒ InvocationEvaluator.callFunction(f, Arguments(Seq(EvaluatedArgument.PositionArg(o, None)))))
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
    val x = this(param)
    FunctionHelpers.safeInterpretAsPath(x) match {
      case Some(path) ⇒ path
      case None ⇒
        val message = s"Invalid argument '${param.name}'. Must be a path, but was a ${x.typeName}"
        throw new ArgumentException(message, locationOpt(param))
    }
  }

  def validatePaths(param: Parameter): Seq[Path] = {
    val x = this(param)
    catching(classOf[EvaluatorException]) opt FunctionHelpers.interpretAsPaths(x) getOrElse (
      throw new ArgumentException(s"Invalid argument '${param.name}', could not interpret as path.", locationOpt(param)))
  }

  object MashInteger {

    def unapply(x: MashValue): Option[Int] = x match {
      case n: MashNumber ⇒ n.asInt
      case _             ⇒ None
    }

  }

  def validateInteger(param: Parameter): Int =
    this(param) match {
      case MashInteger(n) ⇒
        n
      case n: MashNumber ⇒
        val message = s"Invalid argument '${param.name}'. Must be an integer, but was '$n'"
        throw new ArgumentException(message, locationOpt(param))
      case x ⇒
        val message = s"Invalid argument '${param.name}'. Must be an integer, but was a ${x.typeName}"
        throw new ArgumentException(message, locationOpt(param))
    }

  def validateIntegerOrNull(param: Parameter): Option[Int] =
    this(param) match {
      case MashInteger(n) ⇒
        Some(n)
      case MashNull ⇒
        None
      case n: MashNumber ⇒
        val message = s"Invalid argument '${param.name}'. Must be an integer, but was '$n'"
        throw new ArgumentException(message, locationOpt(param))
      case x ⇒
        val message = s"Invalid argument '${param.name}'. Must be an integer, but was a ${x.typeName}"
        throw new ArgumentException(message, locationOpt(param))
    }

}