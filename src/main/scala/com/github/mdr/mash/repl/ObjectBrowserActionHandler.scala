package com.github.mdr.mash.repl

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.lexer.MashLexer.isLegalIdentifier
import com.github.mdr.mash.printer.ObjectTableModelCreator

trait ObjectBrowserActionHandler { self: Repl ⇒
  import ObjectBrowserActions._

  private def updateState(newState: ObjectTableBrowserState) {
    state.objectBrowserStateOpt = Some(newState)
  }

  private def adjustWindowToFit(browserState: ObjectTableBrowserState): ObjectTableBrowserState = {
    val currentRow = browserState.currentRow
    val firstRow = browserState.firstRow
    var newState = browserState

    val delta = currentRow - (firstRow + windowSize - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - currentRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  protected def handleObjectBrowserAction(action: InputAction, browserState: ObjectTableBrowserState) {
    val ObjectTableBrowserState(model, currentRow, _, _, _) = browserState
    action match {
      case NextColumn     ⇒
        val newState = adjustWindowToFit(browserState.adjustCurrentColumn(1))
        updateState(newState)
      case PreviousColumn ⇒
        val newState = adjustWindowToFit(browserState.adjustCurrentColumn(-1))
        updateState(newState)
      case UnfocusColumn  ⇒
        val newState = adjustWindowToFit(browserState.unfocusColumn)
        updateState(newState)
      case FirstColumn    =>
        val newState = adjustWindowToFit(browserState.lastColumn)
        updateState(newState)
      case LastColumn     =>
        val newState = adjustWindowToFit(browserState.firstColumn)
        updateState(newState)
      case NextItem      ⇒
        val newState = adjustWindowToFit(browserState.adjustCurrentRow(1))
        updateState(newState)
      case NextPage      ⇒
        val newRow = math.min(model.objects.size - 1, currentRow + windowSize - 1)
        val newState = adjustWindowToFit(browserState.copy(currentRow = newRow))
        updateState(newState)
      case PreviousItem  ⇒
        val newState = adjustWindowToFit(browserState.adjustCurrentRow(-1))
        updateState(newState)
      case PreviousPage ⇒
        val newRow = math.max(0, currentRow - windowSize - 1)
        val newState = adjustWindowToFit(browserState.copy(currentRow = newRow))
        updateState(newState)
      case ExitBrowser ⇒
        state.objectBrowserStateOpt = None
      case FirstItem ⇒
        val newState = adjustWindowToFit(browserState.copy(currentRow = 0))
        updateState(newState)
      case LastItem ⇒
        val newRow = model.objects.size - 1
        val newState = adjustWindowToFit(browserState.copy(currentRow = newRow))
        updateState(newState)
      case InsertItem ⇒
        handleInsertItem(browserState)
      case ToggleSelected ⇒
        updateState(browserState.toggleMark)
      case Rerender ⇒
        val model = new ObjectTableModelCreator(terminal.info, showSelections = true).create(browserState.model.rawObjects)
        updateState(browserState.copy(model = model))
        previousReplRenderResultOpt = None
      case _ ⇒
    }
  }

  private def handleInsertItem(browserState: ObjectTableBrowserState) {
    val toInsert = getInsertExpression(browserState)
    state.lineBuffer = LineBuffer(toInsert)
    state.objectBrowserStateOpt = None
  }

  private def getInsertExpression(browserState: ObjectTableBrowserState): String = {
    val commandNumber = state.commandNumber - 1
    val command = s"${ReplState.Res}$commandNumber"
    if (browserState.markedRows.isEmpty)
      browserState.currentColumnOpt match {
        case Some(column) if column > 0 =>
          val property = browserState.model.columnNames(column)
          if (isLegalIdentifier(property))
            s"$command[${browserState.currentRow}].$property"
          else
            s"$command[${browserState.currentRow}]['$property']"
        case _ =>
          s"$command[${browserState.currentRow}]"
      }
    else {
      val rows = browserState.markedRows.toSeq.sorted
      val items = rows.map(i ⇒ s"$command[$i]").mkString(", ")
      s"[$items]"
    }
  }

  private def windowSize = terminal.info.rows - 5 // three header rows, a footer row, a status line

}