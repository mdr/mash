package com.github.mdr.mash.inference

import scala.PartialFunction.condOpt
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.ParameterModel

sealed trait TypedArgument

object TypedArgument {

  case class PositionArg(arg: AnnotatedExpr) extends TypedArgument

  case class LongFlag(flag: String, valueOpt: Option[AnnotatedExpr]) extends TypedArgument

  case class ShortFlag(flags: Seq[String]) extends TypedArgument

}

trait TypedArguments {

  def arguments: Seq[TypedArgument]

  def positionArgs: Seq[AnnotatedExpr] = arguments.collect { case TypedArgument.PositionArg(arg) ⇒ arg }

  def argSet: Set[String] = arguments.collect {
    case TypedArgument.ShortFlag(flags)     ⇒ flags
    case TypedArgument.LongFlag(flag, None) ⇒ Seq(flag)
  }.flatten.toSet

  def argValues: Map[String, Option[AnnotatedExpr]] = arguments.collect {
    case TypedArgument.LongFlag(flag, value) ⇒ flag -> value
  }.toMap

  def isProvidedAsNamedArg(name: String): Boolean = argSet.contains(name) || argValues.contains(name)

}

object SimpleTypedArguments {

  private def annotateArg(arg: Argument): TypedArgument = arg match {
    case Argument.PositionArg(e, _)           ⇒ TypedArgument.PositionArg(AnnotatedExpr(Some(e), e.typeOpt))
    case Argument.ShortFlag(flags, _)         ⇒ TypedArgument.ShortFlag(flags)
    case Argument.LongFlag(flag, valueOpt, _) ⇒ TypedArgument.LongFlag(flag, valueOpt.map(e ⇒ AnnotatedExpr(Some(e), e.typeOpt)))
  }

  def from(invocationExpr: InvocationExpr): TypedArguments =
    SimpleTypedArguments(invocationExpr.arguments.map(annotateArg))

}

case class SimpleTypedArguments(arguments: Seq[TypedArgument]) extends TypedArguments
