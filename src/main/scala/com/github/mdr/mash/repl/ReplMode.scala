package com.github.mdr.mash.repl

import com.github.mdr.mash.repl.browser.{ ObjectBrowserStateStack, SingleObjectTableBrowserState, TwoDTableBrowserState }
import com.github.mdr.mash.repl.completions.{ BrowserCompletionState, IncrementalCompletionState }

sealed trait ReplMode

object ReplMode {

  case object Normal extends ReplMode

  case object BrowseCompletions extends ReplMode

  case object IncrementalCompletions extends ReplMode

  case object IncrementalSearch extends ReplMode

  case object ObjectBrowser extends ReplMode {

    case object IncrementalSearch extends ReplMode

    case object ExpressionInput extends ReplMode

  }

  def getMode(state: ReplState): ReplMode =
    state.objectBrowserStateStackOpt match {
      case Some(stack) ⇒ getBrowserMode(stack)
      case None        ⇒
        state.completionStateOpt match {
          case Some(_: IncrementalCompletionState)                      ⇒ ReplMode.IncrementalCompletions
          case Some(_: BrowserCompletionState)                          ⇒ ReplMode.BrowseCompletions
          case None if state.incrementalHistorySearchStateOpt.isDefined ⇒ ReplMode.IncrementalSearch
          case None                                                     ⇒ ReplMode.Normal
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


}