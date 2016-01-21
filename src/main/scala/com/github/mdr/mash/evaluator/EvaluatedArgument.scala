package com.github.mdr.mash.evaluator

import com.github.mdr.mash.parser.AbstractSyntax._

sealed trait EvaluatedArgument {

  def argumentNodeOpt: Option[Argument]

}

object EvaluatedArgument {

  case class PositionArg(value: Any, argumentNodeOpt: Option[Argument]) extends EvaluatedArgument

  case class LongFlag(flag: String, valueOpt: Option[Any], argumentNodeOpt: Option[Argument]) extends EvaluatedArgument

  case class ShortFlag(flags: Seq[String], argumentNodeOpt: Option[Argument]) extends EvaluatedArgument

}
