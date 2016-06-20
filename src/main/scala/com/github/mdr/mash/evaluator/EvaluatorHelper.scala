package com.github.mdr.mash.evaluator

import com.github.mdr.mash.parser.AbstractSyntax.Expr

trait EvaluatorHelper {
  
  protected def sourceLocation(expr: Expr)(implicit context: EvaluationContext): Option[SourceLocation] =
    expr.locationOpt.map(SourceLocation)

}