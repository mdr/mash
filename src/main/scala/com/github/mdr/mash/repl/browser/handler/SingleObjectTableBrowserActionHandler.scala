package com.github.mdr.mash.repl.browser.handler

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl.NormalActions.SelfInsert
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ ExpressionInput, _ }
import com.github.mdr.mash.repl.browser.{ SearchState, SingleObjectTableBrowserState }

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
    commonBrowserActionHandler(browserState)
      .orElse(singleObjectTableBrowserActionHandler(browserState))
      .lift(action)

  private def singleObjectTableBrowserActionHandler(browserState: SingleObjectTableBrowserState): PartialFunction[InputAction, Unit] = {
    case NextItem                      ⇒ updateState(browserState.nextItem(terminalRows))
    case PreviousItem                  ⇒ updateState(browserState.previousItem(terminalRows))
    case NextPage                      ⇒ updateState(browserState.nextPage(terminalRows))
    case PreviousPage                  ⇒ updateState(browserState.previousPage(terminalRows))
    case FirstItem                     ⇒ updateState(browserState.firstItem(terminalRows))
    case LastItem                      ⇒ updateState(browserState.lastItem(terminalRows))
    case ToggleMarked                  ⇒ updateState(browserState.toggleMark)
    case ViewAsTree                    ⇒ viewAsTree(browserState)
    case IncrementalSearch.BeginSearch ⇒ updateState(browserState.beginSearch)
  }

  private def handleIncrementalSearchAction(action: InputAction, browserState: SingleObjectTableBrowserState, searchState: SearchState): Unit = {
    import IncrementalSearch._
    action match {
      case SelfInsert(c)                   ⇒ updateState(browserState.setSearch(searchState.query + c, terminalRows))
      case ToggleCase                      ⇒ updateState(browserState.toggleCase(terminalRows))
      case Unsearch                        ⇒ unsearch(browserState, searchState)
      case NextHit                         ⇒ updateState(browserState.nextHit(terminalRows))
      case PreviousHit                     ⇒ updateState(browserState.previousHit(terminalRows))
      case ExitSearch                      ⇒ updateState(browserState.stopSearching)
      case ExpressionInput.BeginExpression ⇒ updateState(browserState.beginExpression)
      case _                               ⇒
    }
  }

  private def unsearch(browserState: SingleObjectTableBrowserState, searchState: SearchState) =
    if (searchState.query.nonEmpty)
      updateState(browserState.setSearch(searchState.query.init, terminalRows))

}
