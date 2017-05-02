package com.github.mdr.mash.evaluator

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime.MashValue

trait EvaluatorHelper {

  protected def sourceLocation(node: AstNode): Option[SourceLocation] = node.locationOpt

  protected def addLocationToExceptionIfMissing[T <: MashValue](locationOpt: Option[SourceLocation])(p: ⇒ T): T =
    try
      p
    catch {
      case e: EvaluatorException if e.stack.isEmpty ⇒
        throw e.copy(stack = locationOpt.toList.map(loc ⇒ StackTraceItem(Some(loc))))
    }

}