package com.github.mdr.mash.evaluator

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime.MashValue

sealed trait EvaluatedArgument {

  def argumentNodeOpt: Option[Argument]

}

object EvaluatedArgument {

  case class PositionArg(value: MashValue, argumentNodeOpt: Option[Argument]) extends EvaluatedArgument

  case class LongFlag(flag: String, valueOpt: Option[MashValue], argumentNodeOpt: Option[Argument]) extends EvaluatedArgument

  case class ShortFlag(flags: Seq[String], argumentNodeOpt: Option[Argument]) extends EvaluatedArgument

}
