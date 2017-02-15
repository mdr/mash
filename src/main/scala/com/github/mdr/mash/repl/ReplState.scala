package com.github.mdr.mash.repl

import com.github.mdr.mash.ConfigWrapper
import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.printer.ViewConfig
import com.github.mdr.mash.repl.browser.{ ObjectBrowserStateStack, ObjectsTableBrowserState }
import com.github.mdr.mash.repl.completions.{ BrowserCompletionState, CompletionState, IncrementalCompletionState }
import com.github.mdr.mash.repl.history.HistorySearchState
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.utils.Region

case class InsertLastArgState(count: Int, region: Region)

object ReplState {

  /**
    * Name of the 'it' variable, which stores the last result
    */
  val It = "it"

  /**
    * Name of the 'res' list, which stores the list of commands executed in the session.
    */
  val Res = "res"

}

class ReplState(var lineBuffer: LineBuffer = LineBuffer.Empty,
                var commandNumber: Int = 0,
                var completionStateOpt: Option[CompletionState] = None,
                var assistanceStateOpt: Option[AssistanceState] = None,
                var continue: Boolean = true, // Whether to loop or exit
                var globalVariables: MashObject,
                var historySearchStateOpt: Option[HistorySearchState] = None,
                var mish: Boolean = false,
                var insertLastArgStateOpt: Option[InsertLastArgState] = None,
                var objectBrowserStateStackOpt: Option[ObjectBrowserStateStack] = None) {

  def reset() {
    lineBuffer = LineBuffer.Empty
    completionStateOpt = None
    assistanceStateOpt = None
    historySearchStateOpt = None
    insertLastArgStateOpt = None
  }

  def updateLineBuffer(transformation: LineBuffer ⇒ LineBuffer) {
    this.lineBuffer = transformation(this.lineBuffer)
  }

  def mode: ReplMode =
    objectBrowserStateStackOpt match {
      case Some(stack) ⇒
        stack.headState match {
          case s: ObjectsTableBrowserState if s.searchStateOpt.isDefined ⇒ ReplMode.ObjectBrowser.IncrementalSearch
          case s if s.expressionOpt.isDefined                            ⇒ ReplMode.ObjectBrowser.ExpressionInput
          case _                                                         ⇒ ReplMode.ObjectBrowser
        }
      case None        ⇒
        completionStateOpt match {
          case Some(_: IncrementalCompletionState)     ⇒ ReplMode.IncrementalCompletions
          case Some(_: BrowserCompletionState)         ⇒ ReplMode.BrowseCompletions
          case None if historySearchStateOpt.isDefined ⇒ ReplMode.IncrementalSearch
          case None                                    ⇒ ReplMode.Normal
        }
    }

  private def config: ConfigWrapper = ConfigWrapper.fromGlobals(globalVariables)

  def bareWords: Boolean = config.bareWords

  def showStartupTips: Boolean = config.showStartupTips

  def viewConfig: ViewConfig = ViewConfig(config.viewFuzzyTime)
}
