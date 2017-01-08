package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.ConcreteSyntax._

object SafeParens {

  def safeParens(exprString: String): String = {
    val expr = MashParser.parseForgiving(exprString)
    val needsParens = expr match {
      case _: ParenExpr | _: MemberExpr | _: LookupExpr | _: BlockExpr | _: ListExpr | _: ObjectExpr | _: Identifier ⇒ false
      case _ ⇒ true
    }
    addParens(exprString, needsParens)
  }

  private def addParens(s: String, yes: Boolean): String =
    if (yes) s"($s)" else s

  def safeParens(prefix: String, suffix: String): String = {
    val prefixExpr = MashParser.parseForgiving(prefix)
    val suffixExpr = MashParser.parseForgiving("it" + suffix)
    val yes = (prefixExpr, suffixExpr) match {
      case (_: ParenExpr | _: MemberExpr | _: LookupExpr | _: BlockExpr | _: ListExpr | _: ObjectExpr | _: Identifier, _) ⇒ false
      case (_: PipeExpr, _: PipeExpr) ⇒ false
      case _ ⇒ true
    }
    addParens(prefix, yes) + suffix
  }

}
