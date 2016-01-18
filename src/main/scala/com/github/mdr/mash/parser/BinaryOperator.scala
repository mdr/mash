package com.github.mdr.mash.parser

sealed trait BinaryOperator

object BinaryOperator {
  case object Or extends BinaryOperator
  case object And extends BinaryOperator
  case object Equals extends BinaryOperator
  case object NotEquals extends BinaryOperator
  case object GreaterThan extends BinaryOperator
  case object GreaterThanEquals extends BinaryOperator
  case object LessThan extends BinaryOperator
  case object LessThanEquals extends BinaryOperator
  case object Plus extends BinaryOperator
  case object Minus extends BinaryOperator
  case object Multiply extends BinaryOperator
  case object Divide extends BinaryOperator
  case object Sequence extends BinaryOperator

}