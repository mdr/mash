package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.ConcreteSyntax._
import com.github.mdr.mash.utils.Utils._

object SafeParens {

  /**
    * Add parentheses to the given expression if required to allow it to be followed either by a lookup or member expression:
    *  XXX[1], or XXX.field
    */
  def safeParens(exprString: String): String = {
    val expr = MashParser.parseForgiving(exprString).body
    val needsParens = expr match {
      case _: ParenExpr | _: MemberExpr | _: LookupExpr | _: BlockExpr | _: ListExpr | _: ObjectExpr | _: Identifier ⇒ false
      case _                                                                                                         ⇒ true
    }
    exprString.when(needsParens, parenthesise)
  }

  def safeParens(prefix: String, suffix: String): String = {
    val prefixExpr = MashParser.parseForgiving(prefix).body
    val suffixExpr = MashParser.parseForgiving("it" + suffix).body
    val needsParens = (prefixExpr, suffixExpr) match {
      case (_: ParenExpr | _: MemberExpr | _: LookupExpr | _: BlockExpr | _: ListExpr | _: ObjectExpr | _: Identifier, _) ⇒ false
      case (_: PipeExpr, _: PipeExpr)                                                                                     ⇒ false
      case _                                                                                                              ⇒ true
    }
    prefix.when(needsParens, parenthesise) + suffix
  }

  private def parenthesise(s: String) = s"($s)"

}
