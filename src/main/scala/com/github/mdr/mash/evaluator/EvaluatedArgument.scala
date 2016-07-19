package com.github.mdr.mash.evaluator

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime.MashValue

sealed trait EvaluatedArgument {

  def argumentNodeOpt: Option[Argument]
  
  def isPositionArg: Boolean

}

case class SuspendedMashValue(thunk: () => MashValue) {
  def resolve(): MashValue = thunk()
}

object EvaluatedArgument {

  case class PositionArg(value: SuspendedMashValue, argumentNodeOpt: Option[Argument] = None) extends EvaluatedArgument {
    def isPositionArg = true
  }

  case class LongFlag(flag: String, valueOpt: Option[SuspendedMashValue], argumentNodeOpt: Option[Argument]) extends EvaluatedArgument {
    def isPositionArg = false
  }

  case class ShortFlag(flags: Seq[String], argumentNodeOpt: Option[Argument]) extends EvaluatedArgument {
    def isPositionArg = false
  }

}
