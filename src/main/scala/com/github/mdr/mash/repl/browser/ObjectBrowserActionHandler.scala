package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.commands.CommandRunner
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.parser.SafeParens
import com.github.mdr.mash.printer.model._
import com.github.mdr.mash.repl.NormalActions.SelfInsert
import com.github.mdr.mash.repl.{ LineBuffer, _ }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

trait ObjectBrowserActionHandler
  extends TextLinesBrowserActionHandler
    with ValueBrowserActionHandler
    with ObjectTreeBrowserActionHandler
    with SingleObjectTableBrowserActionHandler
    with ObjectsTableBrowserActionHandler {
  self: Repl ⇒

  import ObjectBrowserActions._

  protected def updateState(newState: BrowserState) {
    state.objectBrowserStateStackOpt.foreach { objectBrowserState =>
      state.objectBrowserStateStackOpt = Some(objectBrowserState.replaceCurrentState(newState))
    }
  }

  protected def navigateForward(newState: BrowserState) =
    state.objectBrowserStateStackOpt.foreach { objectBrowserState =>
      state.objectBrowserStateStackOpt = Some(objectBrowserState.pushNewState(newState))
    }

  protected def navigateBack() =
    state.objectBrowserStateStackOpt.foreach { objectBrowserState =>
      state.objectBrowserStateStackOpt = if (objectBrowserState.browserStates.size == 1) None else Some(objectBrowserState.pop)
    }

  protected def focus(browserState: BrowserState, tree: Boolean = false): Unit = {
    val selectionInfo = browserState.selectionInfo
    focus(selectionInfo.rawObject, selectionInfo.path, tree)
  }

  private def focus(value: MashValue, newPath: String, tree: Boolean): Unit =
    navigateForward(
      if (tree && (value.isInstanceOf[MashList] || value.isAnObject)) {
        val model = new ObjectTreeModelCreator(state.viewConfig).create(value)
        ObjectTreeBrowserState.initial(model, newPath)
      } else
        getNewBrowserState(value, newPath))

  protected def viewAsTree(browserState: BrowserState): Unit = {
    val model = new ObjectTreeModelCreator(state.viewConfig).create(browserState.rawValue)
    updateState(ObjectTreeBrowserState.initial(model, browserState.path))
  }

  protected def getNewBrowserState(value: MashValue, path: String): BrowserState = value match {
    case obj: MashObject                                       =>
      val model = new ObjectModelCreator(terminal.info, state.viewConfig).create(obj)
      SingleObjectTableBrowserState(model, path = path)
    case xs: MashList if xs.forall(_.isInstanceOf[MashObject]) ⇒
      val objects = xs.items.asInstanceOf[Seq[MashObject]]
      val model = new ObjectsTableModelCreator(terminal.info, showSelections = true, state.viewConfig).create(objects, xs)
      ObjectsTableBrowserState(model, path = path)
    case xs: MashList                                          =>
      val model = new TextLinesModelCreator(state.viewConfig).create(xs)
      TextLinesBrowserState(model, path = path)
    case _                                                     =>
      val model = new ValueModelCreator(terminal.info, state.viewConfig).create(value)
      ValueBrowserState(model, path = path)
  }

  private def insert(s: String): Unit = {
    state.lineBuffer = LineBuffer(s)
    state.objectBrowserStateStackOpt = None
  }

  protected def handleInsertWholeItem(browserState: BrowserState) =
    insert(browserState.path)

  protected def handleInsertItem(browserState: BrowserState) =
    insert(browserState.getInsertExpression)

  protected def handleObjectBrowserAction(action: InputAction, browserStateStack: ObjectBrowserStateStack): Unit =
    browserStateStack.headState.expressionOpt match {
      case Some(expression) =>
        handleExpressionInputAction(action, browserStateStack.headState, expression)
      case None             =>
        browserStateStack.headState match {
          case objectTableBrowserState: ObjectsTableBrowserState       => handleObjectsTableBrowserAction(action, objectTableBrowserState)
          case singleObjectBrowserState: SingleObjectTableBrowserState => handleSingleObjectBrowserAction(action, singleObjectBrowserState)
          case objectTreeBrowserState: ObjectTreeBrowserState          => handleObjectTreeBrowserAction(action, objectTreeBrowserState)
          case valueBrowserState: ValueBrowserState                    => handleValueBrowserAction(action, valueBrowserState)
          case textLinesBrowserState: TextLinesBrowserState            => handleTextLinesBrowserAction(action, textLinesBrowserState)
          case _                                                       =>
        }
    }

  private def handleExpressionInputAction(action: InputAction, browserState: BrowserState, expression: String): Unit = {
    import ExpressionInput._
    action match {
      case SelfInsert(c)      =>
        updateState(browserState.setExpression(expression + c))
      case BackwardDeleteChar =>
        if (expression.nonEmpty)
          updateState(browserState.setExpression(expression.init))
      case Accept             =>
        val newPath = SafeParens.safeParens(browserState.path, expression)
        val fullExpression = "it" + expression
        val isolatedGlobals = MashObject.of(state.globalVariables.immutableFields + ("it" -> browserState.rawValue))
        val commandRunner = new CommandRunner(output, terminal.info, isolatedGlobals, sessionId, printErrors = false)
        val compilationUnit = CompilationUnit(fullExpression)
        updateState(browserState.acceptExpression)
        for (result <- commandRunner.runCompilationUnit(compilationUnit, state.bareWords))
          focus(result, newPath, tree = false)
      case _                  =>
    }
  }

  protected def handleOpenItem(browserState: BrowserState) = {
    state.lineBuffer = LineBuffer.Empty
    state.objectBrowserStateStackOpt = None
    updateScreenAfterAccept()
    val command = s"${browserState.getInsertExpression} | open"
    runCommand(command)
  }

  protected def terminalRows: Int = terminal.info.rows

}