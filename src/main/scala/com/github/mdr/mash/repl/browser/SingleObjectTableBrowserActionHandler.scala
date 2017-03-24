package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl.NormalActions.SelfInsert
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ ExpressionInput, Focus, _ }
import com.github.mdr.mash.repl.browser.ObjectsTableBrowserState.SearchState

trait SingleObjectTableBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl ⇒

  protected def handleSingleObjectTableBrowserAction(action: InputAction, browserState: SingleObjectTableBrowserState): Unit =
    browserState.searchStateOpt match {
      case Some(searchState) ⇒
        handleIncrementalSearchAction(action, browserState, searchState)
      case None              ⇒
        handleDefaultSingleObjectTableBrowserAction(action, browserState)
    }

  private def handleDefaultSingleObjectTableBrowserAction(action: InputAction, browserState: SingleObjectTableBrowserState): Unit =
    action match {
      case NextItem                        ⇒
        updateState(browserState.nextItem(terminalRows))
      case PreviousItem                    ⇒
        val newState = browserState.previousItem(terminalRows)
        updateState(newState)
      case FirstItem                       ⇒
        updateState(browserState.firstItem(terminalRows))
      case LastItem                        ⇒
        updateState(browserState.lastItem(terminalRows))
      case ExitBrowser                     ⇒
        state.objectBrowserStateStackOpt = None
      case Focus                           ⇒
        focus(browserState)
      case Back                            ⇒
        navigateBack()
      case InsertItem                      ⇒
        handleInsertItem(browserState)
      case InsertWholeItem                 ⇒
        handleInsertWholeItem(browserState)
      case Open                            ⇒
        handleOpenItem(browserState)
      case ViewAsTree                      ⇒
        viewAsTree(browserState)
      case IncrementalSearch.BeginSearch   ⇒
        updateState(browserState.copy(searchStateOpt = Some(SearchState(""))))
      case ExpressionInput.BeginExpression ⇒
        updateState(browserState.setExpression(""))
      case _                               ⇒
    }

  private def handleIncrementalSearchAction(action: InputAction, browserState: SingleObjectTableBrowserState, searchState: SearchState): Unit = {
    import IncrementalSearch._
    action match {
      case SelfInsert(c)                   ⇒
        updateState(browserState.setSearch(searchState.query + c, terminalRows))
      case ToggleCase                      ⇒
        updateState(browserState.toggleCase(terminalRows))
      case Unsearch                        ⇒
        if (searchState.query.nonEmpty)
          updateState(browserState.setSearch(searchState.query.init, terminalRows))
      case NextHit                         ⇒
        updateState(browserState.nextHit(terminalRows))
      case PreviousHit                     ⇒
        updateState(browserState.previousHit(terminalRows))
      case ExitSearch                      ⇒
        updateState(browserState.stopSearching)
      case ExpressionInput.BeginExpression ⇒
        updateState(browserState.setExpression(""))
      case _                               ⇒
    }
  }

}
