package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.repl.{ LineBuffer, LineBufferResult }
import com.github.mdr.mash.repl.completions.CompletionState

case class ExpressionState(lineBuffer: LineBuffer,
                           copiedOpt: Option[String] = None,
                           completionStateOpt: Option[CompletionState] = None,
                           assistanceStateOpt: Option[AssistanceState] = None) {

  def updateLineBuffer(transformation: LineBuffer ⇒ LineBuffer): ExpressionState =
    withLineBuffer(transformation(this.lineBuffer))

  def updateLineBufferResult(transformation: LineBuffer ⇒ LineBufferResult): ExpressionState =
    withLineBuffer(transformation(this.lineBuffer))

  def withLineBuffer(lineBuffer: LineBuffer): ExpressionState = copy(lineBuffer = lineBuffer)

  def withLineBuffer(lineBufferResult: LineBufferResult): ExpressionState =
    copy(lineBuffer = lineBufferResult.lineBuffer, copiedOpt = lineBufferResult.copiedOpt orElse copiedOpt)

}
