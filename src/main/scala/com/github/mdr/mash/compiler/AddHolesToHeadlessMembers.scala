package com.github.mdr.mash.compiler

import com.github.mdr.mash.parser.AbstractSyntax._

object AddHolesToHeadlessMembers {

  def addHoles(expr: Expr): Expr = expr.transform {
    case HeadlessMemberExpr(member, isNullSafe, sourceInfoOpt) ⇒ MemberExpr(Hole(None), member, isNullSafe, sourceInfoOpt)
  }

}