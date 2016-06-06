package com.github.mdr.mash.evaluator

import scala.PartialFunction.cond
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNull

object Truthiness {

  def isFalsey(x: Any): Boolean = cond(x) {
    case false | MashNull | MashNumber(0, _) | MashString("", _) | MashList() ⇒ true
    case MashObject(fields, None) ⇒ fields.isEmpty
  }

  def isTruthy(x: Any): Boolean = !isFalsey(x)

}