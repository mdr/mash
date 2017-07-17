package com.github.mdr.mash.repl

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.repl.browser.{ ObjectBrowserStateStack, SingleObjectTableBrowserState, TwoDTableBrowserState }
import com.github.mdr.mash.repl.completions.{ BrowserCompletionState, CompletionState, IncrementalCompletionState }
import com.github.mdr.mash.repl.history.HistorySearchState
import com.github.mdr.mash.utils.Region

case class InsertLastArgState(count: Int, region: Region)

object ReplState {

  /**
    * Name of the 'it' variable, which stores the last result
    */
  val It = "it"

  /**
    * Name of the results list, which stores the list of commands executed in the session.
    */
  val ResultsListName = "r"

  /**
    * Prefix to give to the variables holding results, e.g. r0, r1
    */
  val ResultVarPrefix = "r"

}

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

  def mode: ReplMode =
    objectBrowserStateStackOpt match {
      case Some(stack) ⇒ getBrowserMode(stack)
      case None        ⇒
        completionStateOpt match {
          case Some(_: IncrementalCompletionState)     ⇒ ReplMode.IncrementalCompletions
          case Some(_: BrowserCompletionState)         ⇒ ReplMode.BrowseCompletions
          case None if historySearchStateOpt.isDefined ⇒ ReplMode.IncrementalSearch
          case None                                    ⇒ ReplMode.Normal
        }
    }

  private def getBrowserMode(stack: ObjectBrowserStateStack): ReplMode = {
    val browserState = stack.headState
    browserState.expressionStateOpt.flatMap(_.completionStateOpt) match {
      case Some(_: BrowserCompletionState) ⇒ ReplMode.BrowseCompletions
      case _                               ⇒
        browserState match {
          case s: TwoDTableBrowserState if s.searchStateOpt.isDefined         ⇒ ReplMode.ObjectBrowser.IncrementalSearch
          case s: SingleObjectTableBrowserState if s.searchStateOpt.isDefined ⇒ ReplMode.ObjectBrowser.IncrementalSearch
          case s if s.expressionStateOpt.isDefined                            ⇒ ReplMode.ObjectBrowser.ExpressionInput
          case _                                                              ⇒ ReplMode.ObjectBrowser
        }
    }
  }

  def incrementCommandNumber: ReplState = copy(commandNumber = commandNumber + 1)

  def exitCompletion: ReplState = copy(completionStateOpt = None)
}
