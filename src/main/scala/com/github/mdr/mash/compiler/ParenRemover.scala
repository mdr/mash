package com.github.mdr.mash.compiler

import com.github.mdr.mash.parser.AbstractSyntax._

object ParenRemover {

  def removeParens(expr: Expr): Expr = expr.transform {
    case ParenExpr(expr, sourceInfoOpt) ⇒ expr
  }

}