package com.github.mdr.mash.evaluator

import com.github.mdr.mash.utils.PointedRegion
import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.parser.Provenance
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.MashCallable

case class SourceLocation(provenance: Provenance, pointedRegion: PointedRegion)

case class StackTraceItem(location: SourceLocation, functionOpt: Option[MashCallable] = None)

case class EvaluatorException(
  message: String,
  stack: List[StackTraceItem] = Nil,
  cause: Throwable = null)
    extends RuntimeException(message, cause) {

  def this(message: String, locationOpt: Option[SourceLocation]) =
    this(message, locationOpt.toList.map(loc ⇒ StackTraceItem(loc)))

  def causeOpt: Option[Throwable] = Option(cause)

  def lastWasFunction(functionOpt: Option[MashCallable]): EvaluatorException = {
    val newStack = stack match {
      case Nil          ⇒ Nil
      case head :: tail ⇒ head.copy(functionOpt = functionOpt) :: tail
    }
    copy(stack = newStack)
  }

  def push(location: SourceLocation): EvaluatorException = copy(stack = StackTraceItem(location) :: stack)

}
