package com.github.mdr.mash.repl.browser

import java.nio.file.{ Files, Paths }

import com.github.mdr.mash.commands.CommandRunner
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.ns.os.PathSummaryClass
import com.github.mdr.mash.parser.SafeParens
import com.github.mdr.mash.printer.model.TwoDTableModelCreator.isSuitableForTwoDTable
import com.github.mdr.mash.printer.model._
import com.github.mdr.mash.repl.NormalActions.SelfInsert
import com.github.mdr.mash.repl.{ LineBuffer, _ }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }

import scala.PartialFunction.condOpt

trait ObjectBrowserActionHandler
  extends TextLinesBrowserActionHandler
    with ValueBrowserActionHandler
    with ObjectTreeBrowserActionHandler
    with SingleObjectTableBrowserActionHandler
    with TwoDTableBrowserActionHandler {
  self: Repl ⇒

  import ObjectBrowserActions._

  protected def updateState(newState: BrowserState) {
    state.objectBrowserStateStackOpt.foreach { objectBrowserState ⇒
      state.objectBrowserStateStackOpt = Some(objectBrowserState.replaceCurrentState(newState))
    }
  }

  protected def navigateForward(newState: BrowserState) =
    state.objectBrowserStateStackOpt.foreach { objectBrowserState ⇒
      state.objectBrowserStateStackOpt = Some(objectBrowserState.pushNewState(newState))
    }

  protected def navigateBack() =
    state.objectBrowserStateStackOpt.foreach { objectBrowserState ⇒
      state.objectBrowserStateStackOpt = if (objectBrowserState.browserStates.size == 1) None else Some(objectBrowserState.pop)
    }

  protected def focus(browserState: BrowserState, tree: Boolean = false): Unit = {
    val selectionInfo = browserState.selectionInfo
    focus(selectionInfo.rawObject, selectionInfo.path, tree)
  }

  protected def focusDirectory(browserState: BrowserState): Unit = {
    val selectionInfo = browserState.selectionInfo
    val isDirectory = condOpt(selectionInfo.rawObject) {
      case s: MashString                                             ⇒ Paths.get(s.s)
      case obj: MashObject if obj.classOpt contains PathSummaryClass ⇒ Paths.get(PathSummaryClass.Wrapper(obj).path)
    }.exists(Files.isDirectory(_))
    if (isDirectory)
      focusExpression(selectionInfo.path, selectionInfo.rawObject, ".children")
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

  protected def view1D(browserState: BrowserState): Unit =
    browserState.rawValue match {
      case obj: MashObject ⇒
        val model = new SingleObjectTableModelCreator(terminal.info, state.viewConfig).create(obj)
        val newState = SingleObjectTableBrowserState(model, path = browserState.path)
        updateState(newState)
      case xs: MashList    ⇒
        val model = new TextLinesModelCreator(state.viewConfig).create(xs)
        val newState = TextLinesBrowserState(model, path = browserState.path)
        updateState(newState)
      case _               ⇒
    }

  protected def view2D(browserState: BrowserState) = {
    def view2D(value: MashValue): Unit = {
      val model = new TwoDTableModelCreator(terminal.info, showSelections = true, state.viewConfig).create(value)
      val newState = TwoDTableBrowserState(model, path = browserState.path)
      updateState(newState)
    }
    browserState.rawValue match {
      case obj: MashObject if obj.immutableFields.values.forall(v ⇒ v.isAnObject || v.isAList) ⇒
        view2D(obj)
      case xs: MashList if xs.forall(x ⇒ x.isAnObject || x.isAList)                            ⇒
        view2D(xs)
      case _                                                                                   ⇒
    }
  }

  protected def getNewBrowserState(value: MashValue, path: String): BrowserState = value match {
    case _ if isSuitableForTwoDTable(value) ⇒
      val model = new TwoDTableModelCreator(terminal.info, showSelections = true, state.viewConfig).create(value)
      TwoDTableBrowserState(model, path = path)
    case obj: MashObject                    ⇒
      val model = new SingleObjectTableModelCreator(terminal.info, state.viewConfig).create(obj)
      SingleObjectTableBrowserState(model, path = path)
    case xs: MashList                       ⇒
      val model = new TextLinesModelCreator(state.viewConfig).create(xs)
      TextLinesBrowserState(model, path = path)
    case _                                  ⇒
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
      case Some(expression) ⇒
        handleExpressionInputAction(action, browserStateStack.headState, expression)
      case None             ⇒
        browserStateStack.headState match {
          case twoDTableBrowserState: TwoDTableBrowserState            ⇒ handleTwoDTableBrowserAction(action, twoDTableBrowserState)
          case singleObjectBrowserState: SingleObjectTableBrowserState ⇒ handleSingleObjectTableBrowserAction(action, singleObjectBrowserState)
          case objectTreeBrowserState: ObjectTreeBrowserState          ⇒ handleObjectTreeBrowserAction(action, objectTreeBrowserState)
          case valueBrowserState: ValueBrowserState                    ⇒ handleValueBrowserAction(action, valueBrowserState)
          case textLinesBrowserState: TextLinesBrowserState            ⇒ handleTextLinesBrowserAction(action, textLinesBrowserState)
          case _                                                       ⇒
        }
    }

  private def handleExpressionInputAction(action: InputAction, browserState: BrowserState, expression: String): Unit = {
    import ExpressionInput._
    action match {
      case SelfInsert(c)      ⇒
        updateState(browserState.setExpression(expression + c))
      case BackwardDeleteChar ⇒
        if (expression.nonEmpty)
          updateState(browserState.setExpression(expression.init))
      case Accept             ⇒
        updateState(browserState.acceptExpression)
        focusExpression(browserState.path, browserState.rawValue, expression)
      case _                  ⇒
    }
  }

  private def focusExpression(currentPath: String, currentValue: MashValue, furtherExpression: String): Unit = {
    val newPath = SafeParens.safeParens(currentPath, furtherExpression)
    val fullExpression = "it" + furtherExpression
    val isolatedGlobals = MashObject.of(state.globalVariables.immutableFields + (MashString("it") -> currentValue))
    val commandRunner = new CommandRunner(output, terminal.info, isolatedGlobals, sessionId, printErrors = false)
    val compilationUnit = CompilationUnit(fullExpression)
    for (result <- commandRunner.runCompilationUnit(compilationUnit, state.bareWords))
      focus(result, newPath, tree = false)
  }

  protected def handleOpenItem(browserState: BrowserState) = {
    state.lineBuffer = LineBuffer.Empty
    state.objectBrowserStateStackOpt = None
    updateScreenAfterAccept()
    val command = s"${browserState.getInsertExpression} | open"
    runCommand(command)
  }

  protected def handleCopyItem(browserState: BrowserState) = {
    val command = s"${browserState.getInsertExpression} | clipboard"
    runCommand(command)
  }

  protected def terminalRows: Int = terminal.info.rows

}
