package com.github.mdr.mash.inference

import com.github.mdr.mash.parser.AbstractSyntax._

sealed trait TypedArgument {
  def valueOpt: Option[ValueInfo]
}

object TypedArgument {

  case class PositionArg(arg: ValueInfo) extends TypedArgument {
    def valueOpt = Some(arg)
  }

  case class LongFlag(flag: String, valueOpt: Option[ValueInfo]) extends TypedArgument


  case class ShortFlag(flags: Seq[String]) extends TypedArgument {
    val valueOpt = None
  }

}

object TypedArguments {

  private def makeTypedArg(arg: Argument): TypedArgument = arg match {
    case Argument.PositionArg(e, _)           ⇒
      TypedArgument.PositionArg(ValueInfo(e.constantValueOpt, e.typeOpt))
    case Argument.ShortFlag(flags, _)         ⇒
      TypedArgument.ShortFlag(flags)
    case Argument.LongFlag(flag, valueOpt, _) ⇒
      TypedArgument.LongFlag(flag, valueOpt.map(e ⇒ ValueInfo(e.constantValueOpt, e.typeOpt)))
  }

  def from(invocationExpr: InvocationExpr): TypedArguments = from(invocationExpr.arguments)

  def from(arguments: Seq[Argument]): TypedArguments = TypedArguments(arguments.map(makeTypedArg))

}

case class TypedArguments(arguments: Seq[TypedArgument] = Seq()) {

  def positionArgs: Seq[ValueInfo] = arguments.collect { case TypedArgument.PositionArg(arg) ⇒ arg }

  def argSet: Set[String] = arguments.collect {
    case TypedArgument.ShortFlag(flags)     ⇒ flags
    case TypedArgument.LongFlag(flag, None) ⇒ Seq(flag)
  }.flatten.toSet

  def argValues: Map[String, Option[ValueInfo]] = arguments.collect {
    case TypedArgument.LongFlag(flag, value) ⇒ flag -> value
  }.toMap

  def isProvidedAsNamedArg(name: String): Boolean = argSet.contains(name) || argValues.contains(name)

}
