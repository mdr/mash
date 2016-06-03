package com.github.mdr.mash.repl

import com.github.mdr.mash.input.InputAction

trait ObjectBrowserActionHandler { self: Repl ⇒
  import ObjectBrowserActions._

  private def updateState(newState: ObjectBrowserState) {
    state.objectBrowserStateOpt = Some(newState)
  }

  private def adjustWindowToFit(browserState: ObjectBrowserState): ObjectBrowserState = {
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

  protected def handleObjectBrowserAction(action: InputAction, browserState: ObjectBrowserState) {
    val ObjectBrowserState(model, currentRow, firstRow) = browserState
    action match {
      case NextItem ⇒
        if (currentRow < model.objects.size - 1) {
          val newState = adjustWindowToFit(browserState.adjustCurrentRow(1))
          updateState(newState)
        }
      case NextPage ⇒
        val newRow = math.min(model.objects.size - 1, currentRow + windowSize - 1)
        val newState = adjustWindowToFit(browserState.copy(currentRow = newRow))
        updateState(newState)
      case PreviousItem ⇒
        if (currentRow > 0) {
          val newState = adjustWindowToFit(browserState.adjustCurrentRow(-1))
          updateState(newState)
        }
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
        val commandNumber = state.commandNumber - 1
        val toInsert = s"${ReplState.Res}[$commandNumber][$currentRow]"
        state.lineBuffer = state.lineBuffer.addCharactersAtCursor(toInsert)
        state.objectBrowserStateOpt = None
      case _ ⇒
    }
  }

  private def windowSize = terminal.info.rows - 5 // three header rows, a footer row, a status line

}