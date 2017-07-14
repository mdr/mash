package com.github.mdr.mash.repl.browser

import java.nio.file.{ Files, Paths }

import com.github.mdr.mash.commands.CommandRunner
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.lexer.MashLexer.isLegalIdentifier
import com.github.mdr.mash.ns.os.PathSummaryClass
import com.github.mdr.mash.parser.ExpressionCombiner
import com.github.mdr.mash.parser.ExpressionCombiner._
import com.github.mdr.mash.parser.LookupDecomposer._
import com.github.mdr.mash.parser.StringEscapes.escapeChars
import com.github.mdr.mash.printer.model._
import com.github.mdr.mash.repl.NormalActions.SelfInsert
import com.github.mdr.mash.repl.{ LineBuffer, _ }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }
import com.github.mdr.mash.utils.Utils.indexOf

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

  protected def focus(value: MashValue, newPath: String, tree: Boolean): Unit =
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
      case obj: MashObject if obj.nonEmpty ⇒
        val model = new SingleObjectTableModelCreator(terminal.info, supportMarking = true, state.viewConfig).create(obj)
        val newState = SingleObjectTableBrowserState(model, path = browserState.path)
        updateState(newState)
      case xs: MashList                    ⇒
        val model = new TextLinesModelCreator(state.viewConfig).create(xs)
        val newState = TextLinesBrowserState(model, path = browserState.path)
        updateState(newState)
      case _                               ⇒
    }

  protected def view2D(browserState: BrowserState) = {
    def view2D(value: MashValue): Unit = {
      val model = new TwoDTableModelCreator(terminal.info, supportMarking = true, state.viewConfig).create(value)
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

  protected def getNewBrowserState(value: MashValue, path: String): BrowserState =
    BrowserState.fromModel(DisplayModel.getDisplayModel(value, state.viewConfig, terminal.info), path)

  private def insert(expression: String): Unit = {
    state.lineBuffer = LineBuffer(expression)
    state.objectBrowserStateStackOpt = None
  }

  protected def handleInsertWholeItem(browserState: BrowserState) =
    insert(browserState.path)

  protected def handleInsertItem(browserState: BrowserState) =
    insert(browserState.getInsertExpression)

  protected def handleObjectBrowserAction(action: InputAction, browserStateStack: ObjectBrowserStateStack): Unit =
    browserStateStack.headState.expressionStateOpt match {
      case Some(expressionState) ⇒
        handleExpressionInputAction(action, browserStateStack.headState, expressionState)
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

  private def handleExpressionInputAction(action: InputAction, browserState: BrowserState, expressionState: ExpressionState) {
    import ExpressionInput._
    val expression = expressionState.expression
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
    val newPath = ExpressionCombiner.combineSafely(currentPath, furtherExpression)
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
    val command = ExpressionCombiner.combineSafely(browserState.getInsertExpression, " | open")
    runCommand(command)
  }

  protected def handleCopyItem(browserState: BrowserState) = {
    val command = ExpressionCombiner.combineSafely(browserState.getInsertExpression, " | clipboard")
    runCommand(command)
  }

  private case class ItemAndPath(item: MashValue, path: String)

  protected def selectParentItem(browserState: BrowserState, delta: Int) = {
    val newItemAndPath = selectParentItemByIntegerIndex(browserState, delta) orElse selectParentItemByName(browserState, delta)
    for (ItemAndPath(newItem, newPath) <- newItemAndPath)
      updateState(getNewBrowserState(newItem, newPath))
  }

  private def getParentValue: Option[MashValue] =
    for {
      stack <- state.objectBrowserStateStackOpt
      parentState ← stack.parentState
    } yield parentState.rawValue

  private def selectParentItemByName(browserState: BrowserState, delta: Int): Option[ItemAndPath] =
    for {
      LookupWithStringIndex(prefix, name) <- decomposeLookupWithStringIndex(browserState.path) orElse decomposeMember(browserState.path)
      parentValue ← getParentValue
      obj ← parentValue.asObject
      fields = obj.immutableFields.keys.toSeq
      i ← indexOf(fields, MashString(name))
      newIndex = (i + delta + fields.size) % fields.size
      newField ← fields(newIndex).asString.map(_.s)
      newItem ← obj.get(newField)
      newPath = combineSafely(prefix, if (isLegalIdentifier(newField)) s".$newField" else s"['${escapeChars(newField)}']")
    } yield ItemAndPath(newItem, newPath)

  private def selectParentItemByIntegerIndex(browserState: BrowserState, delta: Int): Option[ItemAndPath] =
    for {
      LookupWithIntegerIndex(prefix, i) ← decomposeLookupWithIntegerIndex(browserState.path)
      parentValue ← getParentValue
      list ← parentValue.asList
      newIndex = (i + delta + list.size) % list.size
      newItem = list.immutableElements(newIndex)
      newPath = combineSafely(prefix, s"[$newIndex]")
    } yield ItemAndPath(newItem, newPath)

  protected def terminalRows: Int = terminal.info.rows

}
