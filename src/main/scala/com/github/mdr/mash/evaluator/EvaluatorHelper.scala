package com.github.mdr.mash.evaluator

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime.MashValue

trait EvaluatorHelper {

  protected def sourceLocation(expr: AstNode)(implicit context: EvaluationContext): Option[SourceLocation] =
    expr.locationOpt

  protected def addLocationToExceptionIfMissing[T <: MashValue](locationOpt: Option[SourceLocation])(p: ⇒ T): T =
    try
      p
    catch {
      case e: EvaluatorException if e.stack.isEmpty ⇒
        throw e.copy(stack = locationOpt.toList.map(loc => StackTraceItem(loc)))
    }

}