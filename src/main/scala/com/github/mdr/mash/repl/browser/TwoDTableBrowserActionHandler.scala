package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.printer.model._
import com.github.mdr.mash.repl.NormalActions.SelfInsert
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ ExpressionInput, Focus, PreviousPage, _ }
import com.github.mdr.mash.repl.browser.TwoDTableBrowserState.SearchState

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

  protected def handleDefaultTwoDTableBrowserAction(action: InputAction, browserState: TwoDTableBrowserState) =
    action match {
      case NextColumn                      ⇒
        updateState(browserState.nextColumn)
      case PreviousColumn                  ⇒
        updateState(browserState.previousColumn)
      case UnfocusColumn                   ⇒
        val newState = browserState.unfocusColumn
        updateState(newState)
      case FirstColumn                     ⇒
        updateState(browserState.lastColumn)
      case LastColumn                      ⇒
        updateState(browserState.firstColumn)
      case NextItem                        ⇒
        updateState(browserState.nextItem(terminalRows))
      case NextPage                        ⇒
        updateState(browserState.nextPage(terminalRows))
      case PreviousItem                    ⇒
        updateState(browserState.previousItem(terminalRows))
      case PreviousPage                    ⇒
        updateState(browserState.previousPage(terminalRows))
      case ExitBrowser                     ⇒
        state.objectBrowserStateStackOpt = None
      case FirstItem                       ⇒
        updateState(browserState.firstItem(terminalRows))
      case LastItem                        ⇒
        updateState(browserState.lastItem(terminalRows))
      case InsertItem                      ⇒
        handleInsertItem(browserState)
      case InsertWholeItem                 ⇒
        handleInsertWholeItem(browserState)
      case Open                            ⇒
        handleOpenItem(browserState)
      case Back                            ⇒
        navigateBack()
      case Focus                           ⇒
        focus(browserState)
      case FocusDirectory                  ⇒
        focusDirectory(browserState)
      case ToggleMarked                    ⇒
        updateState(browserState.toggleMark)
      case Rerender                        ⇒
        val modelCreator = new TwoDTableModelCreator(terminal.info, showSelections = true, state.viewConfig)
        val model = modelCreator.create(browserState.model.rawValue)
        updateState(browserState.copy(model = model))
        previousReplRenderResultOpt = None
      case ViewAsTree                      ⇒
        viewAsTree(browserState)
      case HideColumn                      ⇒
        handleHideColumn(browserState)
      case IncrementalSearch.BeginSearch   ⇒
        updateState(browserState.copy(searchStateOpt = Some(SearchState(""))))
      case ExpressionInput.BeginExpression ⇒
        updateState(browserState.setExpression(""))
      case _                               ⇒
    }

  private def handleHideColumn(browserState: TwoDTableBrowserState) =
    for (currentColumn <- browserState.currentColumnOpt if currentColumn > 0)
      updateState(hideColumn(browserState, currentColumn))

  private def hideColumn(browserState: TwoDTableBrowserState, currentColumn: Int): TwoDTableBrowserState = {
    val columnId = browserState.model.columnIds(currentColumn)
    val list = browserState.model.rawValue
    val hiddenColumns = browserState.hiddenColumns :+ columnId
    val modelCreator = new TwoDTableModelCreator(terminal.info, showSelections = true, state.viewConfig, hiddenColumns)
    val model = modelCreator.create(list)
    val newColumn = if (currentColumn >= model.numberOfColumns) currentColumn - 1 else currentColumn
    browserState.copy(model = model, hiddenColumns = hiddenColumns, currentColumnOpt = Some(newColumn))
  }

}

