package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.parser.AbstractSyntax.Argument
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.utils.PointedRegion
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluatedArgument
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.evaluator.MashNumber

case class BoundParams(params: Map[String, Any], argumentNodes: Map[String, Argument]) {

  def apply(param: String): Any = params(param)

  def apply(param: Parameter): Any = params(param.name)

  def get(param: Parameter): Option[Any] = params.get(param.name)

  def get(param: String): Option[Any] = params.get(param)

  @throws[EvaluatorException]
  def throwInvalidArgument(param: Parameter, message: String): Nothing = {
    val fullMessage = s"Invalid argument '${param.name}'. $message"
    throw new EvaluatorException(fullMessage, locationOpt(param))
  }

  private def locationOpt(param: Parameter): Option[PointedRegion] =
    argumentNodes.get(param.name).flatMap(_.sourceInfoOpt).map(_.location)

  def validateSequence(param: Parameter): Seq[Any] = this(param) match {
    case xs: Seq[Any]          ⇒ xs
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

  def validateFunction(param: Parameter): Any ⇒ Any =
    this(param) match {
      case f @ (_: MashString | _: MashFunction | _: BoundMethod) ⇒
        (o ⇒ Evaluator.callFunction(f, Arguments(Seq(EvaluatedArgument.PositionArg(o, None)))))
      case x ⇒
        val message = s"Invalid argument '${param.name}'. Must be a function, but was '${ToStringifier.stringify(x)}'"
        throw new EvaluatorException(message, locationOpt(param))
    }

  object MashInteger {

    def unapply(x: Any): Option[Int] = x match {
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
    case null ⇒
      None
    case x ⇒
      val message = s"Invalid argument '${param.name}'. Must be an integer, but was '${ToStringifier.stringify(x)}'"
      throw new EvaluatorException(message, locationOpt(param))
  }

}