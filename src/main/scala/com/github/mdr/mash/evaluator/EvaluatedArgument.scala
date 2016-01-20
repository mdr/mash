package com.github.mdr.mash.evaluator

import com.github.mdr.mash.parser.AbstractSyntax._

sealed trait EvaluatedArgument

object EvaluatedArgument {

  case class PositionArg(value: Any) extends EvaluatedArgument

  case class LongFlag(flag: String, valueOpt: Option[Any]) extends EvaluatedArgument

  case class ShortFlag(flags: Seq[String]) extends EvaluatedArgument

}
