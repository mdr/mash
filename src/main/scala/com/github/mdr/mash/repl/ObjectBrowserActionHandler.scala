package com.github.mdr.mash.repl

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.lexer.MashLexer.isLegalIdentifier
import com.github.mdr.mash.printer.{ ObjectModelCreator, ObjectTableModelCreator }
import com.github.mdr.mash.runtime.{ MashList, MashObject }

trait ObjectBrowserActionHandler {
  self: Repl ⇒

  import ObjectBrowserActions._

  private def updateState(newState: BrowserState) {
    state.objectBrowserStateOpt.foreach { objectBrowserState =>
      state.objectBrowserStateOpt = Some(objectBrowserState.replaceCurrentState(newState))
    }
  }

  private def navigateForward(newState: BrowserState) {
    state.objectBrowserStateOpt.foreach { objectBrowserState =>
      state.objectBrowserStateOpt = Some(objectBrowserState.pushNewState(newState))
    }
  }

  private def navigateBack(): Unit = {
    state.objectBrowserStateOpt.foreach { objectBrowserState =>
      state.objectBrowserStateOpt = if (objectBrowserState.browserStates.size == 1) None else Some(objectBrowserState.pop)
    }
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

  private def adjustWindowToFit(browserState: SingleObjectBrowserState): SingleObjectBrowserState = {
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

  protected def handleObjBrowserAction(action: InputAction, browserState: ObjectBrowserState): Unit =
    browserState.browserState match {
      case objectTableBrowserState: ObjectTableBrowserState   => handleTableObjectBrowserAction(action, objectTableBrowserState)
      case singleObjectBrowserState: SingleObjectBrowserState => handleSingleObjectBrowserAction(action, singleObjectBrowserState)
      case _                                                  =>
    }

  protected def handleSingleObjectBrowserAction(action: InputAction, browserState: SingleObjectBrowserState): Unit = action match {
    case NextItem     ⇒
      val newState = adjustWindowToFit(browserState.adjustCurrentRow(1))
      updateState(newState)
    case PreviousItem ⇒
      val newState = adjustWindowToFit(browserState.adjustCurrentRow(-1))
      updateState(newState)
    case FirstItem    ⇒
      val newState = adjustWindowToFit(browserState.copy(currentRow = 0))
      updateState(newState)
    case LastItem     ⇒
      val newRow = browserState.model.fields.size - 1
      val newState = adjustWindowToFit(browserState.copy(currentRow = newRow))
      updateState(newState)
    case ExitBrowser  ⇒
      state.objectBrowserStateOpt = None
    case Focus        ⇒
      val (field, value) = browserState.model.rawValues.toSeq(browserState.currentRow)
      val newPath = safeProperty(browserState.path, field)
      value match {
        case obj: MashObject                                                      =>
          val model = new ObjectModelCreator(terminal.info).create(obj)
          navigateForward(SingleObjectBrowserState(model, path = newPath))
        case xs: MashList if xs.nonEmpty && xs.forall(_.isInstanceOf[MashObject]) ⇒
          val objects = xs.items.asInstanceOf[Seq[MashObject]]
          val model = new ObjectTableModelCreator(terminal.info, showSelections = true).create(objects)
          navigateForward(ObjectTableBrowserState(model, path = newPath))
        case _                                                                    =>
      }
    case Back         =>
      navigateBack()
    case InsertItem   ⇒
      handleInsertItem(browserState)
    case _            =>

  }

  protected def handleTableObjectBrowserAction(action: InputAction, browserState: ObjectTableBrowserState) {
    val ObjectTableBrowserState(model, currentRow, _, _, _, _) = browserState
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
      case NextItem       ⇒
        val newState = adjustWindowToFit(browserState.adjustCurrentRow(1))
        updateState(newState)
      case NextPage       ⇒
        val newRow = math.min(model.objects.size - 1, currentRow + windowSize - 1)
        val newState = adjustWindowToFit(browserState.copy(currentRow = newRow))
        updateState(newState)
      case PreviousItem   ⇒
        val newState = adjustWindowToFit(browserState.adjustCurrentRow(-1))
        updateState(newState)
      case PreviousPage   ⇒
        val newRow = math.max(0, currentRow - windowSize - 1)
        val newState = adjustWindowToFit(browserState.copy(currentRow = newRow))
        updateState(newState)
      case ExitBrowser    ⇒
        state.objectBrowserStateOpt = None
      case FirstItem      ⇒
        val newState = adjustWindowToFit(browserState.copy(currentRow = 0))
        updateState(newState)
      case LastItem       ⇒
        val newRow = model.objects.size - 1
        val newState = adjustWindowToFit(browserState.copy(currentRow = newRow))
        updateState(newState)
      case InsertItem     ⇒
        handleInsertItem(browserState)
      case Back           =>
        navigateBack()
      case Focus          ⇒
        val rawObject = model.rawObjects(currentRow)
        val (newPath, focusedObject) = browserState.currentColumnOpt match {
          case Some(column) =>
            val field = model.columnNames(column)
            val obj = model.objects(browserState.currentRow).rawObjects.get(field).getOrElse(rawObject)
            val newPath = s"${browserState.path}[${browserState.currentRow}].$field"
            (newPath, obj)
          case None         => (s"${browserState.path}[${browserState.currentRow}]", rawObject)
        }
        focusedObject match {
          case obj: MashObject                                                      =>
            val model = new ObjectModelCreator(terminal.info).create(obj)
            navigateForward(SingleObjectBrowserState(model, path = newPath))
          case xs: MashList if xs.nonEmpty && xs.forall(_.isInstanceOf[MashObject]) ⇒
            val objects = xs.items.asInstanceOf[Seq[MashObject]]
            val model = new ObjectTableModelCreator(terminal.info, showSelections = true).create(objects)
            navigateForward(ObjectTableBrowserState(model, path = newPath))
          case _                                                                    =>
        }

      case ToggleMarked ⇒
        updateState(browserState.toggleMark)
      case Rerender     ⇒
        val model = new ObjectTableModelCreator(terminal.info, showSelections = true).create(browserState.model.rawObjects)
        updateState(browserState.copy(model = model))
        previousReplRenderResultOpt = None
      case _            ⇒
    }
  }

  private def handleInsertItem(browserState: ObjectTableBrowserState) {
    val toInsert = getInsertExpression(browserState)
    state.lineBuffer = LineBuffer(toInsert)
    state.objectBrowserStateOpt = None
  }

  private def handleInsertItem(browserState: SingleObjectBrowserState) {
    val toInsert = getInsertExpression(browserState)
    state.lineBuffer = LineBuffer(toInsert)
    state.objectBrowserStateOpt = None
  }

  private def getInsertExpression(browserState: ObjectTableBrowserState): String = {
    val command = browserState.path
    if (browserState.markedRows.isEmpty) {
      val rowPath = s"$command[${browserState.currentRow}]"
      browserState.currentColumnOpt match {
        case Some(column) if column > 0 =>
          val property = browserState.model.columnNames(column)
          safeProperty(rowPath, property)
        case _                          =>
          s"$rowPath"
      }
    } else {
      val rows = browserState.markedRows.toSeq.sorted
      val items = rows.map(i ⇒ s"$command[$i]").mkString(", ")
      s"[$items]"
    }
  }

  private def safeProperty(path: String, property: String): String =
    if (isLegalIdentifier(property))
      s"$path.$property"
    else
      s"$path['$property']"

  private def getInsertExpression(browserState: SingleObjectBrowserState): String = {
    val field = browserState.model.rawValues.toSeq(browserState.currentRow)._1
    val command = browserState.path
    safeProperty(command, field)
  }

  private def windowSize = terminal.info.rows - 5 // three header rows, a footer row, a status line

}