package com.github.mdr.mash.repl

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.lexer.MashLexer.isLegalIdentifier
import com.github.mdr.mash.printer.{ ObjectModelCreator, ObjectTreeModelCreator, ObjectsTableModelCreator }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

import scala.PartialFunction.condOpt

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

  private def adjustWindowToFit(state: ObjectTreeBrowserState): ObjectTreeBrowserState = {
    val selectedRow = state.selectedRow
    val firstRow = state.firstRow
    var newState = state

    val delta = selectedRow - (firstRow + windowSize - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - selectedRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  private def adjustWindowToFit(state: ObjectsTableBrowserState): ObjectsTableBrowserState = {
    val selectedRow = state.selectedRow
    val firstRow = state.firstRow
    var newState = state

    val delta = selectedRow - (firstRow + windowSize - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - selectedRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  private def adjustWindowToFit(state: SingleObjectTableBrowserState): SingleObjectTableBrowserState = {
    val selectedRow = state.selectedRow
    val firstRow = state.firstRow
    var newState = state

    val delta = selectedRow - (firstRow + windowSize - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - selectedRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  protected def handleObjBrowserAction(action: InputAction, browserState: ObjectBrowserState): Unit =
    browserState.browserState match {
      case objectTableBrowserState: ObjectsTableBrowserState       => handleTableObjectBrowserAction(action, objectTableBrowserState)
      case singleObjectBrowserState: SingleObjectTableBrowserState => handleSingleObjectBrowserAction(action, singleObjectBrowserState)
      case objectTreeBrowserState: ObjectTreeBrowserState          => handleObjectTreeBrowserAction(action, objectTreeBrowserState)
      case _                                                       =>
    }

  protected def handleObjectTreeBrowserAction(action: InputAction, browserState: ObjectTreeBrowserState): Unit = action match {
    case Focus          ⇒
      focus(browserState.getSelectedValue, browserState.getNewPath)
    case ExitBrowser  ⇒
      state.objectBrowserStateOpt = None
    case Back         =>
      navigateBack()
    case NextColumn     ⇒
      val newState = browserState.right
      updateState(newState)
    case PreviousColumn     ⇒
      val newState = browserState.left
      updateState(newState)
    case NextItem     ⇒
      val newState = adjustWindowToFit(browserState.down)
      updateState(newState)
    case PreviousItem ⇒
      val newState = adjustWindowToFit(browserState.up)
      updateState(newState)
    case ViewAsTree =>
      getNonTreeBrowserState(browserState.rawValue, browserState.path).foreach(updateState)
    case InsertItem     ⇒
      handleInsertItem(browserState)
    case _ =>
  }

  private def getNonTreeBrowserState(value: MashValue, path: String): Option[BrowserState] = condOpt(value) {
    case obj: MashObject                                                      =>
      val model = new ObjectModelCreator(terminal.info).create(obj)
      SingleObjectTableBrowserState(model, path = path)
    case xs: MashList if xs.nonEmpty && xs.forall(_.isInstanceOf[MashObject]) ⇒
      val objects = xs.items.asInstanceOf[Seq[MashObject]]
      val model = new ObjectsTableModelCreator(terminal.info, showSelections = true).create(objects, xs)
      ObjectsTableBrowserState(model, path = path)
  }

  private def focus(value: MashValue, newPath: String): Unit =
    getNonTreeBrowserState(value, newPath).foreach(navigateForward)

  private def viewAsTree(browserState: BrowserState): Unit = {
    val model = new ObjectTreeModelCreator().create(browserState.rawValue)
    updateState(ObjectTreeBrowserState.initial(model, browserState.path))
  }

  protected def handleSingleObjectBrowserAction(action: InputAction, browserState: SingleObjectTableBrowserState): Unit = action match {
    case NextItem     ⇒
      val newState = adjustWindowToFit(browserState.adjustSelectedRow(1))
      updateState(newState)
    case PreviousItem ⇒
      val newState = adjustWindowToFit(browserState.adjustSelectedRow(-1))
      updateState(newState)
    case FirstItem    ⇒
      val newState = adjustWindowToFit(browserState.copy(selectedRow = 0))
      updateState(newState)
    case LastItem     ⇒
      val newRow = browserState.model.fields.size - 1
      val newState = adjustWindowToFit(browserState.copy(selectedRow = newRow))
      updateState(newState)
    case ExitBrowser  ⇒
      state.objectBrowserStateOpt = None
    case Focus        ⇒
      val field = browserState.selectedField
      val value = browserState.selectedRawValue
      val newPath = safeProperty(browserState.path, field)
      focus(value, newPath)
    case Back         =>
      navigateBack()
    case InsertItem   ⇒
      handleInsertItem(browserState)
    case ViewAsTree =>
      viewAsTree(browserState)
    case _            =>

  }

  protected def handleTableObjectBrowserAction(action: InputAction, browserState: ObjectsTableBrowserState) {
    val ObjectsTableBrowserState(model, currentRow, _, _, _, _) = browserState
    action match {
      case NextColumn     ⇒
        val newState = adjustWindowToFit(browserState.adjustSelectedColumn(1))
        updateState(newState)
      case PreviousColumn ⇒
        val newState = adjustWindowToFit(browserState.adjustSelectedColumn(-1))
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
        val newState = adjustWindowToFit(browserState.adjustSelectedRow(1))
        updateState(newState)
      case NextPage       ⇒
        val newRow = math.min(model.objects.size - 1, currentRow + windowSize - 1)
        val newState = adjustWindowToFit(browserState.copy(selectedRow = newRow))
        updateState(newState)
      case PreviousItem   ⇒
        val newState = adjustWindowToFit(browserState.adjustSelectedRow(-1))
        updateState(newState)
      case PreviousPage   ⇒
        val newRow = math.max(0, currentRow - windowSize - 1)
        val newState = adjustWindowToFit(browserState.copy(selectedRow = newRow))
        updateState(newState)
      case ExitBrowser    ⇒
        state.objectBrowserStateOpt = None
      case FirstItem      ⇒
        val newState = adjustWindowToFit(browserState.copy(selectedRow = 0))
        updateState(newState)
      case LastItem       ⇒
        val newRow = model.objects.size - 1
        val newState = adjustWindowToFit(browserState.copy(selectedRow = newRow))
        updateState(newState)
      case InsertItem     ⇒
        handleInsertItem(browserState)
      case Back           =>
        navigateBack()
      case Focus          ⇒
        val rawObject = model.rawObjects(currentRow)
        val index = browserState.selectedRow
        val (newPath, focusedObject) = browserState.currentColumnOpt match {
          case Some(column) =>
            val field = model.columnNames(column)
            val obj = model.objects(index).rawObjects.getOrElse(field, rawObject)
            val newPath = s"${browserState.path}[$index].$field"
            (newPath, obj)
          case None         => (s"${browserState.path}[$index]", rawObject)
        }
        focus(focusedObject, newPath)
      case ToggleMarked   ⇒
        updateState(browserState.toggleMark)
      case Rerender       ⇒
        val model = new ObjectsTableModelCreator(terminal.info, showSelections = true).create(browserState.model.rawObjects, browserState.model.rawValue)
        updateState(browserState.copy(model = model))
        previousReplRenderResultOpt = None
      case ViewAsTree =>
        viewAsTree(browserState)
      case _              ⇒
    }
  }

  private def handleInsertItem(browserState: ObjectsTableBrowserState) {
    val toInsert = getInsertExpression(browserState)
    state.lineBuffer = LineBuffer(toInsert)
    state.objectBrowserStateOpt = None
  }

  private def handleInsertItem(browserState: SingleObjectTableBrowserState) {
    val toInsert = getInsertExpression(browserState)
    state.lineBuffer = LineBuffer(toInsert)
    state.objectBrowserStateOpt = None
  }

  private def handleInsertItem(browserState: ObjectTreeBrowserState) {
    state.lineBuffer = LineBuffer(browserState.getNewPath)
    state.objectBrowserStateOpt = None
  }

  private def getInsertExpression(browserState: ObjectsTableBrowserState): String = {
    val command = browserState.path
    if (browserState.markedRows.isEmpty) {
      val rowPath = s"$command[${browserState.selectedRow}]"
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

  private def getInsertExpression(browserState: SingleObjectTableBrowserState): String = {
    val field = browserState.model.rawValues.toSeq(browserState.selectedRow)._1
    val command = browserState.path
    safeProperty(command, field)
  }

  private def windowSize = terminal.info.rows - 5 // three header rows, a footer row, a status line

}