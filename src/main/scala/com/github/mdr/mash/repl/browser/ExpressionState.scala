package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.repl.{LineBuffer, ReplState}
import com.github.mdr.mash.repl.completions.CompletionState
import com.github.mdr.mash.utils.Region

case class ExpressionState(lineBuffer: LineBuffer,
                           copiedOpt: Option[String] = None,
                           oldSelections: Seq[Region] = Seq(),
                           completionStateOpt: Option[CompletionState] = None,
                           assistanceStateOpt: Option[AssistanceState] = None) {

  def withCopied(text: String) = copy(copiedOpt = Some(text))

  def updateLineBuffer(transformation: LineBuffer ⇒ LineBuffer): ExpressionState =
    withLineBuffer(transformation(this.lineBuffer))

  def withLineBuffer(lineBuffer: LineBuffer): ExpressionState = copy(lineBuffer = lineBuffer)

  def pushSelection(region: Region): ExpressionState =
    updateLineBuffer(_.withSelection(region))
      .copy(oldSelections = lineBuffer.selectedOrCursorRegion +: oldSelections)

  def popSelection: ExpressionState = oldSelections match {
    case Seq()               ⇒ this
    case Seq(first, rest@_*) ⇒ updateLineBuffer(_.withSelection(first)).copy(oldSelections = rest)
  }

}
