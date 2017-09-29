package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.MashCallable
import com.github.mdr.mash.parser.Provenance
import com.github.mdr.mash.utils.{ LineInfo, Point, PointedRegion }

case class SourceLocation(provenance: Provenance, pointedRegion: PointedRegion) {

  def source = pointedRegion.of(provenance.source)

  def reindentedSource: String = {
    val lineInfo = new LineInfo(provenance.source)
    val Point(lineIndex, column) = lineInfo.lineAndColumn(pointedRegion.region.offset)
    val indented = lineInfo.line(lineIndex).take(column).forall(_ == ' ')
    if (indented)
      reindent(source, column)
    else
      source
  }

  private def reindent(source: String, indent: Int): String = {
    val lineInfo = new LineInfo(source)
    val sourceLines = lineInfo.lines
    val firstLine = sourceLines.take(1)
    val laterLines = sourceLines.drop(1).map { line ⇒
      if (line.take(indent).forall(_ == ' '))
        line.drop(indent)
      else
        line
    }
    (firstLine ++ laterLines).mkString("\n")
  }


}

case class StackTraceItem(locationOpt: Option[SourceLocation], functionOpt: Option[MashCallable] = None)

object EvaluatorException {

  def apply(message: String, locationOpt: Option[SourceLocation]): EvaluatorException =
    EvaluatorException(message, List(StackTraceItem(locationOpt)))

}

case class EvaluatorException(
  message: String,
  stack: List[StackTraceItem] = Nil,
  cause: Throwable = null)
    extends RuntimeException(message, cause) {

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
