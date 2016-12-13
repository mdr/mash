package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.commands.CommandRunner
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.parser.SafeParens
import com.github.mdr.mash.printer.model._
import com.github.mdr.mash.repl.NormalActions.SelfInsert
import com.github.mdr.mash.repl.browser.ObjectsTableBrowserState.SearchState
import com.github.mdr.mash.repl.{ LineBuffer, _ }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

trait ObjectBrowserActionHandler {
  self: Repl ⇒

  import ObjectBrowserActions._

  private def updateState(newState: BrowserState) {
    state.objectBrowserStateOpt.foreach { objectBrowserState =>
      state.objectBrowserStateOpt = Some(objectBrowserState.replaceCurrentState(newState))
    }
  }

  private def navigateForward(newState: BrowserState) =
    state.objectBrowserStateOpt.foreach { objectBrowserState =>
      state.objectBrowserStateOpt = Some(objectBrowserState.pushNewState(newState))
    }

  private def navigateBack() =
    state.objectBrowserStateOpt.foreach { objectBrowserState =>
      state.objectBrowserStateOpt = if (objectBrowserState.browserStates.size == 1) None else Some(objectBrowserState.pop)
    }

  private def treeBrowserWindowSize = terminal.info.rows - 2 // 2 status rows

  private def adjustWindowToFit(state: ObjectTreeBrowserState): ObjectTreeBrowserState = {
    val selectedRow = state.selectedRow
    val firstRow = state.firstRow
    var newState = state

    val delta = selectedRow - (firstRow + treeBrowserWindowSize - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - selectedRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  private def adjustWindowToFit(state: ObjectsTableBrowserState): ObjectsTableBrowserState =
    state.adjustWindowToFit(objectsBrowserWindowSize)

  private def singleObjectWindowSize = terminal.info.rows - 4 // 1 header row, 1 footer row, 2 status rows

  private def adjustWindowToFit(state: SingleObjectTableBrowserState): SingleObjectTableBrowserState = {
    val selectedRow = state.selectedRow
    val firstRow = state.firstRow
    var newState = state

    val delta = selectedRow - (firstRow + singleObjectWindowSize - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - selectedRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  protected def handleObjBrowserAction(action: InputAction, browserState: ObjectBrowserState): Unit =
    browserState.browserState match {
      case objectTableBrowserState: ObjectsTableBrowserState       => handleObjectsTableBrowserAction(action, objectTableBrowserState)
      case singleObjectBrowserState: SingleObjectTableBrowserState => handleSingleObjectBrowserAction(action, singleObjectBrowserState)
      case objectTreeBrowserState: ObjectTreeBrowserState          => handleObjectTreeBrowserAction(action, objectTreeBrowserState)
      case valueBrowserState: ValueBrowserState                    => handleValueBrowserAction(action, valueBrowserState)
      case textLinesBrowserState: TextLinesBrowserState            => handleTextLinesBrowserAction(action, textLinesBrowserState)
      case _                                                       =>
    }


  private val textLinesWindowSize = terminal.info.rows - 2

  protected def handleTextLinesBrowserAction(action: InputAction, browserState: TextLinesBrowserState): Unit = action match {
    case ExitBrowser    ⇒
      state.objectBrowserStateOpt = None
    case Back           =>
      navigateBack()
    case Open =>
      handleOpenItem(browserState)
    case InsertItem =>
      handleInsertItem(browserState)
    case InsertWholeItem     ⇒
      handleInsertWholeItem(browserState)
    case NextItem       ⇒
      val newState = browserState.adjustSelectedRow(1, textLinesWindowSize)
      updateState(newState)
    case PreviousItem   ⇒
      val newState = browserState.adjustSelectedRow(-1, textLinesWindowSize)
      updateState(newState)
    case FirstItem    ⇒
      val newState = browserState.copy(selectedRow = 0).adjustWindowToFit(textLinesWindowSize)
      updateState(newState)
    case LastItem     ⇒
      val newRow = browserState.model.renderedLines.size - 1
      val newState = browserState.copy(selectedRow = newRow).adjustWindowToFit(textLinesWindowSize)
      updateState(newState)
    case NextPage       ⇒
      val newRow = math.min(browserState.model.renderedLines.size - 1, browserState.selectedRow + textLinesWindowSize - 1)
      val newState = browserState.copy(selectedRow = newRow).adjustWindowToFit(textLinesWindowSize)
      updateState(newState)
    case PreviousPage   ⇒
      val newRow = math.max(0, browserState.selectedRow - textLinesWindowSize - 1)
      val newState = browserState.copy(selectedRow = newRow).adjustWindowToFit(textLinesWindowSize)
      updateState(newState)
    case Focus          ⇒
      val rawObject = browserState.model.rawValue.items(browserState.selectedRow)
      focus(rawObject, browserState.getInsertExpression, tree = false)
    case _ =>
  }

  protected def handleValueBrowserAction(action: InputAction, browserState: ValueBrowserState): Unit = action match {
    case ExitBrowser    ⇒
      state.objectBrowserStateOpt = None
    case Back           =>
      navigateBack()
    case InsertItem     ⇒
      handleInsertItem(browserState)
    case Open =>
      handleOpenItem(browserState)
    case InsertWholeItem     ⇒
      handleInsertWholeItem(browserState)
    case _ =>
  }

  protected def handleObjectTreeBrowserAction(action: InputAction, browserState: ObjectTreeBrowserState): Unit = action match {
    case Focus          ⇒
      focus(browserState.getSelectedValue, browserState.getNewPath, tree = true)
    case ExitBrowser    ⇒
      state.objectBrowserStateOpt = None
    case Back           =>
      navigateBack()
    case NextColumn     ⇒
      val newState = browserState.right
      updateState(newState)
    case PreviousColumn ⇒
      val newState = browserState.left
      updateState(newState)
    case NextItem       ⇒
      val newState = adjustWindowToFit(browserState.down)
      updateState(newState)
    case PreviousItem   ⇒
      val newState = adjustWindowToFit(browserState.up)
      updateState(newState)
    case ViewAsTree     =>
      updateState(getNewBrowserState(browserState.rawValue, browserState.path))
    case InsertItem     ⇒
      handleInsertItem(browserState)
    case Open =>
      handleOpenItem(browserState)
    case InsertWholeItem     ⇒
      handleInsertWholeItem(browserState)
    case _              =>
  }

  protected def getNewBrowserState(value: MashValue, path: String): BrowserState = value match {
    case obj: MashObject                                                      =>
      val model = new ObjectModelCreator(terminal.info, state.viewConfig).create(obj)
      SingleObjectTableBrowserState(model, path = path)
    case xs: MashList if xs.forall(_.isInstanceOf[MashObject]) ⇒
      val objects = xs.items.asInstanceOf[Seq[MashObject]]
      val model = new ObjectsTableModelCreator(terminal.info, showSelections = true, state.viewConfig).create(objects, xs)
      ObjectsTableBrowserState(model, path = path)
    case xs: MashList if xs.forall(x => x.isAString || x.isNull) =>
      val model = new TextLinesModelCreator(state.viewConfig).create(xs)
      TextLinesBrowserState(model, path = path)
    case _ =>
      val model = new ValueModelCreator(terminal.info, state.viewConfig).create(value)
      ValueBrowserState(model, path = path)
  }

  private def focus(value: MashValue, newPath: String, tree: Boolean): Unit = {
    navigateForward(
      if (tree && (value.isInstanceOf[MashList] || value.isAnObject)) {
        val model = new ObjectTreeModelCreator(state.viewConfig).create(value)
        ObjectTreeBrowserState.initial(model, newPath)
      } else
        getNewBrowserState(value, newPath))
  }

  private def viewAsTree(browserState: BrowserState): Unit = {
    val model = new ObjectTreeModelCreator(state.viewConfig).create(browserState.rawValue)
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
      val newPath = BrowserState.safeProperty(browserState.path, field)
      focus(value, newPath, tree = false)
    case Back         =>
      navigateBack()
    case InsertItem   ⇒
      handleInsertItem(browserState)
    case InsertWholeItem     ⇒
      handleInsertWholeItem(browserState)
    case Open =>
      handleOpenItem(browserState)
    case ViewAsTree   =>
      viewAsTree(browserState)
    case _            =>

  }

  protected def handleObjectsTableBrowserAction(action: InputAction, browserState: ObjectsTableBrowserState) =
    browserState.searchStateOpt match {
      case Some(searchState) =>
        handleIncrementalSearchAction(action, browserState, searchState)
      case None              =>
        browserState.expressionOpt match {
          case Some(expression) =>
            handleExpressionInputAction(action, browserState, expression)
          case None             =>
            handleDefaultObjectsTableBrowserAction(action, browserState)
        }
    }

  private def handleExpressionInputAction(action: InputAction, browserState: ObjectsTableBrowserState, expression: String): Unit = {
    import ExpressionInput._
    action match {
      case SelfInsert(c) =>
        updateState(browserState.setExpression(expression + c))
      case BackwardDeleteChar =>
        if (expression.nonEmpty)
          updateState(browserState.setExpression(expression.init))
      case Accept =>
        val newPath = SafeParens.safeParens(browserState.path, expression)
        val fullExpression = "it" + expression
        val isolatedGlobals = MashObject.of(state.globalVariables.immutableFields + ("it" -> browserState.rawValue))
        val commandRunner = new CommandRunner(output, terminal.info, isolatedGlobals, sessionId, printErrors = false)
        val compilationUnit = CompilationUnit(fullExpression)
        updateState(browserState.acceptExpression)
        for (result <- commandRunner.runCompilationUnit(compilationUnit, state.bareWords))
          focus(result, newPath, tree = false)
      case _ =>
    }
  }

  private def handleIncrementalSearchAction(action: InputAction, browserState: ObjectsTableBrowserState, searchState: SearchState): Unit = {
    import IncrementalSearch._
    action match {
      case SelfInsert(c) =>
        updateState(browserState.setSearch(searchState.query + c, objectsBrowserWindowSize))
      case ToggleCase    =>
        updateState(browserState.toggleCase(objectsBrowserWindowSize))
      case Unsearch      =>
        if (searchState.query.nonEmpty)
          updateState(browserState.setSearch(searchState.query.init, objectsBrowserWindowSize))
      case NextHit       =>
        updateState(browserState.nextHit(objectsBrowserWindowSize))
      case PreviousHit   =>
        updateState(browserState.previousHit(objectsBrowserWindowSize))
      case ExitSearch    =>
        updateState(browserState.stopSearching)
      case _             =>
    }
  }

  protected def handleDefaultObjectsTableBrowserAction(action: InputAction, browserState: ObjectsTableBrowserState) = {
    val model = browserState.model
    val currentRow = browserState.selectedRow
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
        val newRow = math.min(model.objects.size - 1, currentRow + objectsBrowserWindowSize - 1)
        val newState = adjustWindowToFit(browserState.copy(selectedRow = newRow))
        updateState(newState)
      case PreviousItem   ⇒
        val newState = adjustWindowToFit(browserState.adjustSelectedRow(-1))
        updateState(newState)
      case PreviousPage   ⇒
        val newRow = math.max(0, currentRow - objectsBrowserWindowSize - 1)
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
      case InsertWholeItem     ⇒
        handleInsertWholeItem(browserState)
      case Open =>
        handleOpenItem(browserState)
      case Back           =>
        navigateBack()
      case Focus          ⇒
        val rawObject = model.rawObjects(currentRow)
        val index = browserState.selectedRow
        val safePath = SafeParens.safeParens(browserState.path)
        val (newPath, focusedObject) = browserState.currentColumnOpt match {
          case Some(column) =>
            val field = model.columnNames(column)
            val obj = model.objects(index).rawObjects.getOrElse(field, rawObject)
            val newPath = s"$safePath[$index].$field"
            (newPath, obj)
          case None         => (s"$safePath[$index]", rawObject)
        }
        focus(focusedObject, newPath, tree = false)
      case ToggleMarked   ⇒
        updateState(browserState.toggleMark)
      case Rerender       ⇒
        val modelCreator = new ObjectsTableModelCreator(terminal.info, showSelections = true, state.viewConfig)
        val model = modelCreator.create(browserState.model.rawObjects, browserState.model.rawValue)
        updateState(browserState.copy(model = model))
        previousReplRenderResultOpt = None
      case ViewAsTree     =>
        viewAsTree(browserState)
      case HideColumn =>
        handleHideColumn(browserState)
      case IncrementalSearch.BeginSearch =>
        updateState(browserState.copy(searchStateOpt = Some(SearchState(""))))
      case ExpressionInput.BeginExpression =>
        updateState(browserState.setExpression(""))
      case _              ⇒
    }
  }

  private def handleHideColumn(browserState: ObjectsTableBrowserState) {
    for (currentColumn <- browserState.currentColumnOpt if currentColumn > 0) {
      val columnName = browserState.model.columnNames(currentColumn)
      val list = browserState.model.rawValue
      val objects = browserState.model.rawObjects
      val hiddenColumns = browserState.hiddenColumns :+ columnName
      val modelCreator = new ObjectsTableModelCreator(terminal.info, showSelections = true, state.viewConfig, hiddenColumns)
      val model = modelCreator.create(objects, list)
      val newColumn = if (currentColumn >= model.numberOfColumns) currentColumn - 1 else currentColumn
      val newState = browserState.copy(model = model, hiddenColumns = hiddenColumns, currentColumnOpt = Some(newColumn))
      updateState(newState)
    }
  }

  private def insert(s: String): Unit = {
    state.lineBuffer = LineBuffer(s)
    state.objectBrowserStateOpt = None
  }

  private def handleInsertWholeItem(browserState: BrowserState) =
    insert(browserState.path)

  private def handleInsertItem(browserState: BrowserState) =
    insert(browserState.getInsertExpression)

  private def handleOpenItem(browserState: BrowserState) = {
    state.lineBuffer = LineBuffer.Empty
    state.objectBrowserStateOpt = None
    updateScreenAfterAccept()
    val command = s"${browserState.getInsertExpression} | open"
    runCommand(command)
  }

  private def objectsBrowserWindowSize = terminal.info.rows - 6 // three header rows, a footer row, two status lines

}