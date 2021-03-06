package com.github.mdr.mash.repl.browser.handler

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.view.model._
import com.github.mdr.mash.repl.NormalActions.SelfInsert
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ ExpressionInput, PreviousPage, _ }
import com.github.mdr.mash.repl.browser.{ SearchState, TwoDTableBrowserState }

trait TwoDTableBrowserActionHandler {
  self: ObjectBrowserActionHandler with Repl ⇒

  protected def handleTwoDTableBrowserAction(action: InputAction, browserState: TwoDTableBrowserState) =
    browserState.searchStateOpt match {
      case Some(searchState) ⇒
        handleIncrementalSearchAction(action, browserState, searchState)
      case None              ⇒
        handleDefaultTwoDTableBrowserAction(action, browserState)
    }

  private def handleIncrementalSearchAction(action: InputAction, browserState: TwoDTableBrowserState, searchState: SearchState): Unit = {
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

  private def twoDTableBrowserActionHandler(browserState: TwoDTableBrowserState): PartialFunction[InputAction, Unit] = {
    case NextColumn                      ⇒ updateState(browserState.nextColumn)
    case PreviousColumn                  ⇒ updateState(browserState.previousColumn)
    case UnfocusColumn                   ⇒ updateState(browserState.unfocusColumn)
    case FirstColumn                     ⇒ updateState(browserState.lastColumn)
    case LastColumn                      ⇒ updateState(browserState.firstColumn)
    case NextItem                        ⇒ updateState(browserState.nextItem(terminalRows))
    case NextPage                        ⇒ updateState(browserState.nextPage(terminalRows))
    case PreviousItem                    ⇒ updateState(browserState.previousItem(terminalRows))
    case PreviousPage                    ⇒ updateState(browserState.previousPage(terminalRows))
    case FirstItem                       ⇒ updateState(browserState.firstItem(terminalRows))
    case LastItem                        ⇒ updateState(browserState.lastItem(terminalRows))
    case ToggleMarked                    ⇒ updateState(browserState.toggleMark)
    case ViewAsTree                      ⇒ viewAsTree(browserState)
    case HideColumn                      ⇒ handleHideColumn(browserState)
    case IncrementalSearch.BeginSearch   ⇒ updateState(browserState.beginSearch)
  }

  protected def handleDefaultTwoDTableBrowserAction(action: InputAction, browserState: TwoDTableBrowserState): Unit =
    commonBrowserActionHandler(browserState)
      .orElse(twoDTableBrowserActionHandler(browserState))
      .lift(action)

  private def unsearch(browserState: TwoDTableBrowserState, searchState: SearchState) =
    if (searchState.query.nonEmpty)
      updateState(browserState.setSearch(searchState.query.init, terminalRows))

  private def handleHideColumn(browserState: TwoDTableBrowserState) =
    for (currentColumn ← browserState.currentColumnOpt if currentColumn > 0)
      updateState(hideColumn(browserState, currentColumn))

  private def hideColumn(browserState: TwoDTableBrowserState, currentColumn: Int): TwoDTableBrowserState = {
    val columnId = browserState.model.columnIds(currentColumn)
    val newHiddenColumns = browserState.hiddenColumns :+ columnId
    val modelCreator = new TwoDTableModelCreator(terminal.size, supportMarking = true, viewConfig, newHiddenColumns)
    val newModel = modelCreator.create(browserState.model.rawValue)
    val newCurrentColumn = if (currentColumn >= newModel.numberOfColumns) currentColumn - 1 else currentColumn
    browserState.copy(model = newModel, hiddenColumns = newHiddenColumns, currentColumnOpt = Some(newCurrentColumn))
  }

}
