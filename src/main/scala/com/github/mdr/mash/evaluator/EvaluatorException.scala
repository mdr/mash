package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.MashCallable
import com.github.mdr.mash.parser.Provenance
import com.github.mdr.mash.utils.PointedRegion

case class SourceLocation(provenance: Provenance, pointedRegion: PointedRegion) {

  def source = pointedRegion.of(provenance.source)

}

case class StackTraceItem(locationOpt: Option[SourceLocation], functionOpt: Option[MashCallable] = None)

case class EvaluatorException(
  message: String,
  stack: List[StackTraceItem] = Nil,
  cause: Throwable = null)
    extends RuntimeException(message, cause) {

  def this(message: String, locationOpt: Option[SourceLocation]) =
    this(message, List(StackTraceItem(locationOpt)))

  def causeOpt: Option[Throwable] = Option(cause)

  def lastWasFunction(functionOpt: Option[MashCallable]): EvaluatorException = {
    val newStack = stack match {
      case Nil          ⇒ Nil
      case head :: tail ⇒ head.copy(functionOpt = functionOpt) :: tail
    }
    copy(stack = newStack)
  }

  def push(locationOpt: Option[SourceLocation]): EvaluatorException =
    copy(stack = StackTraceItem(locationOpt) :: stack)

}
