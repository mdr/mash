package com.github.mdr.mash.compiler

import com.github.mdr.mash.parser.AbstractSyntax._

object DesugarPipes {

  def desugarPipes(program: Program): Program = program.transform {
    case PipeExpr(left, InvocationExpr(function: Expr, args, _, _), sourceInfoOpt) ⇒
      InvocationExpr(function, args :+ Argument.PositionArg(left, left.sourceInfoOpt), isParenInvocation = false, sourceInfoOpt)
    case PipeExpr(left, right, sourceInfoOpt) ⇒
      InvocationExpr(right, Seq(Argument.PositionArg(left, left.sourceInfoOpt)), isParenInvocation = false, sourceInfoOpt)
  }.asInstanceOf[Program]

}