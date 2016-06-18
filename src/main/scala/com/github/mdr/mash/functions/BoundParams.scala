package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.parser.AbstractSyntax.Argument
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.utils.PointedRegion
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluatedArgument
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.runtime.MashNumber
import java.nio.file.Path
import com.github.mdr.mash.runtime.MashList
import scala.util.control.Exception._
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.evaluator.InvocationEvaluator

case class BoundParams(params: Map[String, MashValue], argumentNodes: Map[String, Seq[Argument]]) {

  def apply(param: String): MashValue = params(param)

  def apply(param: Parameter): MashValue = params(param.name)

  def get(param: Parameter): Option[MashValue] = params.get(param.name)

  def get(param: String): Option[MashValue] = params.get(param)

  @throws[EvaluatorException]
  def throwInvalidArgument(param: Parameter, message: String): Nothing = {
    val fullMessage = s"Invalid argument '${param.name}'. $message"
    throw new EvaluatorException(fullMessage, locationOpt(param))
  }

  private def locationOpt(param: Parameter): Option[PointedRegion] =
    argumentNodes.get(param.name).map(nodes ⇒ nodes.flatMap(_.sourceInfoOpt).map(_.location).reduce(_ merge _))

  def validateSequence(param: Parameter): Seq[MashValue] = this(param) match {
    case xs: MashList          ⇒ xs.items
    case MashString(s, tagOpt) ⇒ s.toSeq.map(c ⇒ MashString(c.toString, tagOpt))
    case x ⇒
      val message = s"Invalid argument '${param.name}'. Must be a sequence, but was '${ToStringifier.stringify(x)}'"
      throw new EvaluatorException(message, locationOpt(param))
  }

  def validateString(param: Parameter): MashString = this(param) match {
    case s: MashString ⇒ s
    case x ⇒
      val message = s"Invalid argument '${param.name}'. Must be a string, but was '${ToStringifier.stringify(x)}'"
      throw new EvaluatorException(message, locationOpt(param))
  }

  def validateStringOpt(param: Parameter): Option[MashString] = this(param) match {
    case s: MashString ⇒ Some(s)
    case MashNull      ⇒ None
    case x ⇒
      val message = s"Invalid argument '${param.name}'. Must be a string, but was '${ToStringifier.stringify(x)}'"
      throw new EvaluatorException(message, locationOpt(param))
  }

  def validateFunction(param: Parameter): MashValue ⇒ MashValue =
    this(param) match {
      case f @ (_: MashString | _: MashFunction | _: BoundMethod) ⇒
        (o ⇒ InvocationEvaluator.callFunction(f, Arguments(Seq(EvaluatedArgument.PositionArg(o, None)))))
      case x ⇒
        val message = s"Invalid argument '${param.name}'. Must be a function, but was '${ToStringifier.stringify(x)}'"
        throw new EvaluatorException(message, locationOpt(param))
    }

  def validatePath(param: Parameter): Path = {
    val x = this(param)
    FunctionHelpers.safeInterpretAsPath(x) match {
      case Some(path) ⇒ path
      case None ⇒
        val message = s"Invalid argument '${param.name}'. Must be a path, but was '${ToStringifier.stringify(x)}'"
        throw new EvaluatorException(message, locationOpt(param))
    }
  }

  def validatePaths(param: Parameter): Seq[Path] = {
    val x = this(param)
    catching(classOf[EvaluatorException]) opt FunctionHelpers.interpretAsPaths(x) getOrElse (
      throw new EvaluatorException(s"Invalid argument '${param.name}', could not interpret as path.", locationOpt(param)))
  }

  object MashInteger {

    def unapply(x: MashValue): Option[Int] = x match {
      case n: MashNumber ⇒ n.asInt
      case _             ⇒ None
    }

  }

  def validateInteger(param: Parameter): Int = this(param) match {
    case MashInteger(n) ⇒
      n
    case x ⇒
      val message = s"Invalid argument '${param.name}'. Must be an integer, but was '${ToStringifier.stringify(x)}'"
      throw new EvaluatorException(message, locationOpt(param))
  }

  def validateIntegerOrNull(param: Parameter): Option[Int] = this(param) match {
    case MashInteger(n) ⇒
      Some(n)
    case MashNull ⇒
      None
    case x ⇒
      val message = s"Invalid argument '${param.name}'. Must be an integer, but was '${ToStringifier.stringify(x)}'"
      throw new EvaluatorException(message, locationOpt(param))
  }

}