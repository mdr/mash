package com.github.mdr.mash.repl.browser.handler

import java.nio.file.{ Files, Path, Paths }

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.lexer.MashLexer.isLegalIdentifier
import com.github.mdr.mash.ns.os.PathSummaryClass
import com.github.mdr.mash.parser.ExpressionCombiner._
import com.github.mdr.mash.parser.LookupDecomposer._
import com.github.mdr.mash.parser.StringEscapes
import com.github.mdr.mash.parser.StringEscapes.escapeChars
import com.github.mdr.mash.view.model._
import com.github.mdr.mash.repl.NormalActions.RedrawScreen
import com.github.mdr.mash.repl.browser.ObjectBrowserActions._
import com.github.mdr.mash.repl.browser.{ TwoDTableBrowserState, ValueBrowserState, _ }
import com.github.mdr.mash.repl.{ LineBuffer, _ }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }
import com.github.mdr.mash.utils.Utils.indexOf

import scala.PartialFunction.condOpt

object ObjectBrowserActionHandler {

  def getSelectedPath(browserState: BrowserState): Option[Path] =
    for {
      selectionInfo ← browserState.selectionInfoOpt
      path ← getPath(selectionInfo.rawObject)
    } yield path

  private def getPath(value: MashValue): Option[Path] =
    condOpt(value) {
      case s: MashString                                             ⇒ s.s
      case obj: MashObject if obj.classOpt contains PathSummaryClass ⇒ PathSummaryClass.Wrapper(obj).path
    }.map(Paths.get(_))

}

trait ObjectBrowserActionHandler
  extends TextLinesBrowserActionHandler
    with ValueBrowserActionHandler
    with ObjectTreeBrowserActionHandler
    with SingleObjectTableBrowserActionHandler
    with TwoDTableBrowserActionHandler
    with HelpBrowserActionHandler
    with ExpressionActionHandler {
  self: Repl ⇒

  import ObjectBrowserActionHandler._
  
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

  protected def focus(browserState: BrowserState, tree: Boolean = false): Unit =
    for (selectionInfo ← browserState.selectionInfoOpt)
      focus(selectionInfo.rawObject, selectionInfo.path, tree)

  protected def focusDirectory(browserState: BrowserState): Unit =
    for {
      path ← getSelectedPath(browserState)
      if Files.isDirectory(path)
      escapedPath = StringEscapes.escapeChars(path.toString)
    } acceptReplacementExpression(s""""$escapedPath".children""")

  protected def readFile(browserState: BrowserState) =
    for {
      path ← getSelectedPath(browserState)
      if Files.isRegularFile(path)
      escapedPath = StringEscapes.escapeChars(path.toString)
    } acceptReplacementExpression(s""""$escapedPath".readLines""")
  
  protected def focus(value: MashValue, path: String, tree: Boolean): Unit =
    navigateForward(getNewBrowserState(value, path, tree))

  protected def viewAsTree(browserState: BrowserState): Unit =
    updateState(makeObjectTreeBrowserState(browserState.rawValue, browserState.path))

  private def makeObjectTreeBrowserState(value: MashValue, path: String): ObjectTreeBrowserState = {
    val model = new ObjectTreeModelCreator(viewConfig).create(value)
    ObjectTreeBrowserState.initial(model, path)
  }

  protected def view1D(browserState: BrowserState): Unit =
    browserState.rawValue match {
      case obj: MashObject if obj.nonEmpty ⇒
        val model = createSingleObjectTableModel(obj)
        val newState = SingleObjectTableBrowserState(model, path = browserState.path)
        updateState(newState)
      case xs: MashList                    ⇒
        val model = new TextLinesModelCreator(viewConfig).create(xs)
        val newState = TextLinesBrowserState(model, path = browserState.path)
        updateState(newState)
      case _                               ⇒
    }

  protected def createSingleObjectTableModel(obj: MashObject): SingleObjectTableModel =
    new SingleObjectTableModelCreator(terminal.size, supportMarking = true, viewConfig).create(obj)

  protected def view2D(browserState: BrowserState) = {
    def view2D(value: MashValue) {
      val model = createTwoDModel(value)
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

  protected def createTwoDModel(value: MashValue): TwoDTableModel =
    new TwoDTableModelCreator(terminal.size, supportMarking = true, viewConfig).create(value)

  protected def getNewBrowserState(value: MashValue, path: String): BrowserState =
    BrowserState.fromModel(DisplayModel.getDisplayModel(value, viewConfig, terminal.size), path)

  protected def getNewBrowserState(value: MashValue, path: String, tree: Boolean): BrowserState =
    if (tree && (value.isAList || value.isAnObject))
      makeObjectTreeBrowserState(value, path)
    else
      getNewBrowserState(value, path)

  private def insert(expression: String): Unit =
    state = state.copy(
      lineBuffer = state.lineBuffer.insertAtCursor(expression),
      objectBrowserStateStackOpt = None)

  protected def handleInsertWholeItem(browserState: BrowserState) = insert(browserState.path)

  protected def handleInsertItem(browserState: BrowserState) =
    for (expression ← browserState.getInsertExpressionOpt)
      insert(expression)

  protected def handleObjectBrowserAction(action: InputAction, browserStateStack: ObjectBrowserStateStack): Unit =
    if (action == RedrawScreen)
      handleRedrawScreen()
    else if (action == Rerender)
      rerender(browserStateStack.headState)
    else
      browserStateStack.headState.expressionStateOpt match {
        case Some(expressionState) ⇒
          val context = Context(browserStateStack.headState, expressionState)
          handleBrowserExpressionInputAction(action, context)
        case None                  ⇒
          browserStateStack.headState match {
            case browserState: TwoDTableBrowserState         ⇒ handleTwoDTableBrowserAction(action, browserState)
            case browserState: SingleObjectTableBrowserState ⇒ handleSingleObjectTableBrowserAction(action, browserState)
            case browserState: ObjectTreeBrowserState        ⇒ handleObjectTreeBrowserAction(action, browserState)
            case browserState: ValueBrowserState             ⇒ handleValueBrowserAction(action, browserState)
            case browserState: TextLinesBrowserState         ⇒ handleTextLinesBrowserAction(action, browserState)
            case browserState: HelpBrowserState              ⇒ handleHelpBrowserAction(action, browserState)
            case browserState                                ⇒ throw new AssertionError("Unexpected browser state: " + browserState)
          }
      }

  private def rerender(browserState: BrowserState) {
    browserState match {
      case browserState: SingleObjectTableBrowserState ⇒
        val newModel = createSingleObjectTableModel(browserState.model.rawValue)
        updateState(browserState.copy(model = newModel))
      case browserState: TwoDTableBrowserState         ⇒
        val newModel = createTwoDModel(browserState.model.rawValue)
        updateState(browserState.copy(model = newModel))
      case _                                           ⇒
    }
    clearScreen()
    previousScreenOpt = None
  }

  protected def handleOpenItem(browserState: BrowserState) =
    for (expression ← browserState.getInsertExpressionOpt) {
      val command = combineSafely(expression, " | open")
      runCommandQuietly(command)
    }

  protected def handleCopyItem(browserState: BrowserState) =
    for (expression ← browserState.getInsertExpressionOpt) {
      val command = combineSafely(expression, " | clipboard")
      runCommandQuietly(command)
    }

  private case class ItemAndPath(item: MashValue, path: String)

  protected def selectParentItem(browserState: BrowserState, delta: Int, tree: Boolean = false) = {
    val newItemAndPath = selectParentItemByIntegerIndex(browserState, delta) orElse selectParentItemByName(browserState, delta)
    for (ItemAndPath(newItem, newPath) ← newItemAndPath)
      updateState(getNewBrowserState(newItem, newPath, tree))
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

  protected def commonBrowserActionHandler(browserState: BrowserState): PartialFunction[InputAction, Unit] = {
    case ExitBrowser                     ⇒ state = state.copy(objectBrowserStateStackOpt = None)
    case NextParentItem                  ⇒ selectParentItem(browserState, delta = 1)
    case PreviousParentItem              ⇒ selectParentItem(browserState, delta = -1)
    case Open                            ⇒ handleOpenItem(browserState)
    case Copy                            ⇒ handleCopyItem(browserState)
    case Back                            ⇒ navigateBack()
    case Focus                           ⇒ focus(browserState)
    case FocusDirectory                  ⇒ focusDirectory(browserState)
    case ReadFile                        ⇒ readFile(browserState)
    case InsertItem                      ⇒ handleInsertItem(browserState)
    case InsertWholeItem                 ⇒ handleInsertWholeItem(browserState)
    case ExpressionInput.BeginExpression ⇒ updateState(browserState.beginExpression)
    case View1D                          ⇒ view1D(browserState)
    case View2D                          ⇒ view2D(browserState)
  }

}
