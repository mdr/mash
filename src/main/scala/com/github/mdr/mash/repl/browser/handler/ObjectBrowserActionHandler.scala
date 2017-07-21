package com.github.mdr.mash.repl.browser.handler

import java.nio.file.{ Files, Paths }

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.lexer.MashLexer.isLegalIdentifier
import com.github.mdr.mash.ns.os.PathSummaryClass
import com.github.mdr.mash.parser.ExpressionCombiner._
import com.github.mdr.mash.parser.LookupDecomposer._
import com.github.mdr.mash.parser.StringEscapes.escapeChars
import com.github.mdr.mash.printer.model._
import com.github.mdr.mash.repl.browser.{ TwoDTableBrowserState, ValueBrowserState, _ }
import com.github.mdr.mash.repl.{ LineBuffer, _ }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }
import com.github.mdr.mash.utils.Utils.indexOf

import scala.PartialFunction.condOpt

trait ObjectBrowserActionHandler
  extends TextLinesBrowserActionHandler
    with ValueBrowserActionHandler
    with ObjectTreeBrowserActionHandler
    with SingleObjectTableBrowserActionHandler
    with TwoDTableBrowserActionHandler
    with ExpressionActionHandler {
  self: Repl ⇒

  private def updateObjectBrowserStateStack(f: ObjectBrowserStateStack ⇒ Option[ObjectBrowserStateStack]) =
    state.objectBrowserStateStackOpt.foreach { stack ⇒
      state = state.copy(objectBrowserStateStackOpt = f(stack))
    }

  protected def updateState(newState: BrowserState) = updateObjectBrowserStateStack { stack ⇒
    Some(stack.replaceCurrentState(newState))
  }

  protected def navigateForward(newState: BrowserState) =
    updateObjectBrowserStateStack { stack ⇒
      Some(stack.pushNewState(newState))
    }

  protected def navigateBack() = updateObjectBrowserStateStack(_.pop)

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
      acceptFollowOnExpression(selectionInfo.path, selectionInfo.rawObject, ".children")
  }

  protected def focus(value: MashValue, path: String, tree: Boolean): Unit = {
    val newBrowserState =
      if (tree && (value.isAList || value.isAnObject))
        makeObjectTreeBrowserState(value, path)
      else
        getNewBrowserState(value, path)
    navigateForward(newBrowserState)
  }

  protected def viewAsTree(browserState: BrowserState): Unit =
    updateState(makeObjectTreeBrowserState(browserState.rawValue, browserState.path))

  private def makeObjectTreeBrowserState(value: MashValue, path: String): ObjectTreeBrowserState = {
    val model = new ObjectTreeModelCreator(viewConfig).create(value)
    ObjectTreeBrowserState.initial(model, path)
  }

  protected def view1D(browserState: BrowserState): Unit =
    browserState.rawValue match {
      case obj: MashObject if obj.nonEmpty ⇒
        val model = new SingleObjectTableModelCreator(terminal.size, supportMarking = true, viewConfig).create(obj)
        val newState = SingleObjectTableBrowserState(model, path = browserState.path)
        updateState(newState)
      case xs: MashList                    ⇒
        val model = new TextLinesModelCreator(viewConfig).create(xs)
        val newState = TextLinesBrowserState(model, path = browserState.path)
        updateState(newState)
      case _                               ⇒
    }

  protected def view2D(browserState: BrowserState) = {
    def view2D(value: MashValue): Unit = {
      val model = new TwoDTableModelCreator(terminal.size, supportMarking = true, viewConfig).create(value)
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
    BrowserState.fromModel(DisplayModel.getDisplayModel(value, viewConfig, terminal.size), path)

  private def insert(expression: String): Unit = {
    state = state.copy(
      lineBuffer = LineBuffer(expression),
      objectBrowserStateStackOpt = None)
  }

  protected def handleInsertWholeItem(browserState: BrowserState) = insert(browserState.path)

  protected def handleInsertItem(browserState: BrowserState) = insert(browserState.getInsertExpression)

  protected def handleObjectBrowserAction(action: InputAction, browserStateStack: ObjectBrowserStateStack): Unit =
    browserStateStack.headState.expressionStateOpt match {
      case Some(expressionState) ⇒
        handleBrowserExpressionInputAction(action, browserStateStack.headState, expressionState)
      case None                  ⇒
        browserStateStack.headState match {
          case twoDTableBrowserState: TwoDTableBrowserState            ⇒ handleTwoDTableBrowserAction(action, twoDTableBrowserState)
          case singleObjectBrowserState: SingleObjectTableBrowserState ⇒ handleSingleObjectTableBrowserAction(action, singleObjectBrowserState)
          case objectTreeBrowserState: ObjectTreeBrowserState          ⇒ handleObjectTreeBrowserAction(action, objectTreeBrowserState)
          case valueBrowserState: ValueBrowserState                    ⇒ handleValueBrowserAction(action, valueBrowserState)
          case textLinesBrowserState: TextLinesBrowserState            ⇒ handleTextLinesBrowserAction(action, textLinesBrowserState)
          case _                                                       ⇒
        }
    }

  protected def handleOpenItem(browserState: BrowserState) = {
    state = state.copy(
      lineBuffer = LineBuffer.Empty,
      objectBrowserStateStackOpt = None)
    updateScreenAfterAccept()
    val command = combineSafely(browserState.getInsertExpression, " | open")
    runCommand(command)
  }

  protected def handleCopyItem(browserState: BrowserState) = {
    val command = combineSafely(browserState.getInsertExpression, " | clipboard")
    runCommand(command)
  }

  private case class ItemAndPath(item: MashValue, path: String)

  protected def selectParentItem(browserState: BrowserState, delta: Int) = {
    val newItemAndPath = selectParentItemByIntegerIndex(browserState, delta) orElse selectParentItemByName(browserState, delta)
    for (ItemAndPath(newItem, newPath) ← newItemAndPath)
      updateState(getNewBrowserState(newItem, newPath))
  }

  private def getParentValue: Option[MashValue] =
    for {
      stack ← state.objectBrowserStateStackOpt
      parentState ← stack.parentState
    } yield parentState.rawValue

  private def selectParentItemByName(browserState: BrowserState, delta: Int): Option[ItemAndPath] =
    for {
      LookupWithStringIndex(prefix, name) ← decomposeLookupWithStringIndex(browserState.path) orElse decomposeMember(browserState.path)
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

  protected def terminalRows: Int = terminal.size.rows

}
