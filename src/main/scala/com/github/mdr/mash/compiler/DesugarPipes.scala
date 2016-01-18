package com.github.mdr.mash.compiler

import com.github.mdr.mash.parser.AbstractSyntax._

object DesugarPipes {

  def desugarPipes(expr: Expr): Expr = expr.transform {
    case PipeExpr(left, InvocationExpr(function: Expr, args, _), sourceInfoOpt) ⇒
      InvocationExpr(function, args :+ Argument.PositionArg(left), sourceInfoOpt)
    case PipeExpr(left, right, sourceInfoOpt) ⇒
      InvocationExpr(right, Seq(Argument.PositionArg(left)), sourceInfoOpt)
  }

}