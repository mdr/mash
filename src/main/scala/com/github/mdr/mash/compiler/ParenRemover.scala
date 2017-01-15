package com.github.mdr.mash.compiler

import com.github.mdr.mash.parser.AbstractSyntax._

object ParenRemover {

  def removeParens(program: Program): Program = program.transform {
    case ParenExpr(expr, sourceInfoOpt) ⇒ expr
  }.asInstanceOf[Program]

}