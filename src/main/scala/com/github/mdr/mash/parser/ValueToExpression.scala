package com.github.mdr.mash.parser

import com.github.mdr.mash.evaluator.ToStringifier.stringify
import com.github.mdr.mash.parser.StringEscapes.escapeChars
import com.github.mdr.mash.runtime._

import scala.PartialFunction._

object ValueToExpression {

  def apply(value: MashValue): Option[String] = condOpt(value) {
    case MashNull | _: MashBoolean | _: MashNumber ⇒ stringify(value)
    case s: MashString                             ⇒ s"'${escapeChars(s.s)}'"
  }

}
