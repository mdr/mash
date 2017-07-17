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

class ReplState(var lineBuffer: LineBuffer = LineBuffer.Empty,
                var commandNumber_ : Int = 0,
                var completionStateOpt_ : Option[CompletionState] = None,
                var assistanceStateOpt_ : Option[AssistanceState] = None,
                var continue_ : Boolean = true, // Whether to loop or exit
                var historySearchStateOpt_ : Option[HistorySearchState] = None,
                var mish_ : Boolean = false,
                var insertLastArgStateOpt_ : Option[InsertLastArgState] = None,
                var objectBrowserStateStackOpt_ : Option[ObjectBrowserStateStack] = None) {

  def continue = continue_

  def commandNumber = commandNumber_

  def completionStateOpt = completionStateOpt_

  def assistanceStateOpt = assistanceStateOpt_

  def historySearchStateOpt = historySearchStateOpt_

  def mish = mish_

  def insertLastArgStateOpt = insertLastArgStateOpt_

  def objectBrowserStateStackOpt = objectBrowserStateStackOpt_

  def cloneFrom(that: ReplState): Unit = {
    this.lineBuffer = that.lineBuffer
    this.commandNumber_ = that.commandNumber
    this.completionStateOpt_ = that.completionStateOpt
    this.assistanceStateOpt_ = that.assistanceStateOpt
    this.continue_ = that.continue
    this.historySearchStateOpt_ = that.historySearchStateOpt
    this.mish_ = that.mish
    this.insertLastArgStateOpt_ = that.insertLastArgStateOpt
    this.objectBrowserStateStackOpt_ = that.objectBrowserStateStackOpt
  }

  def copy(lineBuffer: LineBuffer = this.lineBuffer,
           commandNumber: Int = this.commandNumber,
           completionStateOpt: Option[CompletionState] = this.completionStateOpt,
           assistanceStateOpt: Option[AssistanceState] = this.assistanceStateOpt,
           continue: Boolean = this.continue,
           historySearchStateOpt: Option[HistorySearchState] = this.historySearchStateOpt,
           mish: Boolean = this.mish,
           insertLastArgStateOpt: Option[InsertLastArgState] = this.insertLastArgStateOpt,
           objectBrowserStateStackOpt: Option[ObjectBrowserStateStack] = this.objectBrowserStateStackOpt): ReplState =
    new ReplState(lineBuffer, commandNumber, completionStateOpt, assistanceStateOpt, continue, historySearchStateOpt,
      mish, insertLastArgStateOpt, objectBrowserStateStackOpt)

  def reset: ReplState = copy(
    lineBuffer = LineBuffer.Empty,
    completionStateOpt = None,
    assistanceStateOpt = None,
    historySearchStateOpt = None,
    insertLastArgStateOpt = None)

  def updateLineBuffer(transformation: LineBuffer ⇒ LineBuffer) {
    this.lineBuffer = transformation(this.lineBuffer)
  }

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
