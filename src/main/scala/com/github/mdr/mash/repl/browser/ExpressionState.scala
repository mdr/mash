package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.repl.LineBuffer
import com.github.mdr.mash.repl.completions.CompletionState

case class ExpressionState(lineBuffer: LineBuffer,
                           completionStateOpt: Option[CompletionState] = None,
                           assistanceStateOpt: Option[AssistanceState] = None) {

  def updateLineBuffer(transformation: LineBuffer ⇒ LineBuffer): ExpressionState =
    withLineBuffer(transformation(this.lineBuffer))

  def withLineBuffer(lineBuffer: LineBuffer): ExpressionState = copy(lineBuffer = lineBuffer)

}
