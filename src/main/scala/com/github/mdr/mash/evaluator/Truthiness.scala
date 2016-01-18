package com.github.mdr.mash.evaluator

import scala.PartialFunction.cond

object Truthiness {

  def isFalsey(x: Any): Boolean = cond(x) {
    case false | null | MashNumber(0, _) | MashString("", _) | Seq() ⇒ true
    case MashObject(fields, None)                                    ⇒ fields.isEmpty
  }

  def isTruthy(x: Any): Boolean = !isFalsey(x)

}