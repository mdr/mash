package com.github.mdr.mash.evaluator

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime.MashValue

sealed trait EvaluatedArgument[+T] {

  def argumentNodeOpt: Option[Argument]
  
  def isPositionArg: Boolean

  def map[U](f: T ⇒ U): EvaluatedArgument[U]

}

object EvaluatedArgument {

  case class PositionArg[T](value: T, argumentNodeOpt: Option[Argument] = None) extends EvaluatedArgument[T] {
    def isPositionArg = true
    def map[U](f: T ⇒ U) = PositionArg(f(value), argumentNodeOpt)
  }

  case class LongFlag[T](flag: String, valueOpt: Option[T], argumentNodeOpt: Option[Argument] = None) extends EvaluatedArgument[T] {
    def isPositionArg = false
    def map[U](f: T ⇒ U) = LongFlag(flag, valueOpt.map(f), argumentNodeOpt)
  }

  case class ShortFlag(flags: Seq[String], argumentNodeOpt: Option[Argument] = None) extends EvaluatedArgument[Nothing] {
    def isPositionArg = false
    def map[U](f: Nothing ⇒ U) = ShortFlag(flags, argumentNodeOpt)
  }

}

case class SuspendedMashValue(thunk: () ⇒ MashValue) {
  def resolve(): MashValue = thunk()
}
