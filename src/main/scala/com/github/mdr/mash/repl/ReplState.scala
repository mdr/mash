package com.github.mdr.mash.repl

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.repl.browser.ObjectBrowserStateStack
import com.github.mdr.mash.repl.completions.CompletionState
import com.github.mdr.mash.repl.history.HistorySearchState
import com.github.mdr.mash.utils.Region

case class InsertLastArgState(count: Int, region: Region)

case class ReplState(lineBuffer: LineBuffer = LineBuffer.Empty,
                     commandNumber: Int = 0,
                     completionStateOpt: Option[CompletionState] = None,
                     assistanceStateOpt: Option[AssistanceState] = None,
                     continue: Boolean = true, // Whether to loop or exit
                     historySearchStateOpt: Option[HistorySearchState] = None,
                     mish: Boolean = false,
                     insertLastArgStateOpt: Option[InsertLastArgState] = None,
                     objectBrowserStateStackOpt: Option[ObjectBrowserStateStack] = None) {


  def reset: ReplState = copy(
    lineBuffer = LineBuffer.Empty,
    completionStateOpt = None,
    assistanceStateOpt = None,
    historySearchStateOpt = None,
    insertLastArgStateOpt = None)

  def updateLineBuffer(transformation: LineBuffer ⇒ LineBuffer): ReplState =
    copy(lineBuffer = transformation(this.lineBuffer))

  def incrementCommandNumber: ReplState = copy(commandNumber = commandNumber + 1)

  def exitCompletion: ReplState = copy(completionStateOpt = None)
}
