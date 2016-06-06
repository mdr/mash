package com.github.mdr.mash.evaluator

import scala.PartialFunction.cond
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.runtime.MashValue

object Truthiness {

  def isFalsey(x: MashValue): Boolean = cond(x) {
    case MashBoolean.False | MashNull | MashNumber(0, _) | MashString("", _) | MashList() ⇒ true
    case MashObject(fields, None) ⇒ fields.isEmpty
  }

  def isTruthy(x: MashValue): Boolean = !isFalsey(x)

}