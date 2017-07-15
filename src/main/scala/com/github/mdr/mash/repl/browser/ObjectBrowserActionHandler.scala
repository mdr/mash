package com.github.mdr.mash.repl.browser

import java.nio.file.{ Files, Paths }

import com.github.mdr.mash.commands.CommandRunner
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.completions.{ Completion, CompletionResult }
import com.github.mdr.mash.editor.QuoteToggler
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.lexer.MashLexer.isLegalIdentifier
import com.github.mdr.mash.ns.os.PathSummaryClass
import com.github.mdr.mash.parser.ExpressionCombiner._
import com.github.mdr.mash.parser.LookupDecomposer._
import com.github.mdr.mash.parser.StringEscapes.escapeChars
import com.github.mdr.mash.printer.model._
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.completions.{ IncrementalCompletionState, ReplStateMemento }
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

  private def updateObjectBrowserStateStack(f: ObjectBrowserStateStack ⇒ Option[ObjectBrowserStateStack]) =
    state.objectBrowserStateStackOpt.foreach { stack ⇒ state.objectBrowserStateStackOpt = f(stack) }

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
    val model = new ObjectTreeModelCreator(state.viewConfig).create(value)
    ObjectTreeBrowserState.initial(model, path)
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

  private def handleIncrementalCompletionAction(action: InputAction,
                                                browserState: BrowserState,
                                                expressionState: ExpressionState,
                                                completionState: IncrementalCompletionState) =
    action match {
      case SelfInsert(s)                                              ⇒
        val (newLineBuffer, newCompletionStateOpt) = getNewIncrementalSearchState(s, expressionState.lineBuffer, completionState, mish = false)
        val newExpressionState = expressionState.copy(completionStateOpt = newCompletionStateOpt, lineBuffer = newLineBuffer)
        updateState(browserState.setExpression(newExpressionState))
      case BackwardDeleteChar if completionState.mementoOpt.isDefined ⇒ // restore previous state
        for (ReplStateMemento(lineBuffer, completionState) ← completionState.mementoOpt) {
          val newExpressionState = expressionState.copy(completionStateOpt = Some(completionState), lineBuffer = lineBuffer)
          updateState(browserState.setExpression(newExpressionState))
        }
      case Complete if completionState.immediatelyAfterCompletion     ⇒ // enter browse completions mode
        val (newCompletionState, newLineBuffer) = getBrowseCompletionState(completionState, activeCompletion = 0, state.lineBuffer)
        val newExpressionState = expressionState.copy(completionStateOpt = Some(newCompletionState), lineBuffer = newLineBuffer)
        updateState(browserState.setExpression(newExpressionState))
      case _                                                          ⇒ // exit back to normal mode, and handle there
        val newExpressionState = expressionState.copy(completionStateOpt = None)
        updateState(browserState.setExpression(newExpressionState))
        handleNormalExpressionInputAction(action, browserState.setExpression(newExpressionState), newExpressionState)
    }


  private def handleExpressionInputAction(action: InputAction, browserState: BrowserState, expressionState: ExpressionState) =
    expressionState.completionStateOpt match {
      case Some(completionState: IncrementalCompletionState) ⇒ handleIncrementalCompletionAction(action, browserState, expressionState, completionState)
      case _                                                 ⇒ handleNormalExpressionInputAction(action, browserState, expressionState)
    }

  private def handleNormalExpressionInputAction(action: InputAction, browserState: BrowserState, expressionState: ExpressionState) {
    def updateExpressionBuffer(f: LineBuffer ⇒ LineBuffer) =
      updateState(browserState.setExpression(expressionState.copy(lineBuffer = f(expressionState.lineBuffer))))
    action match {
      case SelfInsert(c)      ⇒ updateExpressionBuffer(_.addCharactersAtCursor(c))
      case BeginningOfLine    ⇒ updateExpressionBuffer(_.moveCursorToStart)
      case EndOfLine          ⇒ updateExpressionBuffer(_.moveCursorToEnd)
      case ForwardChar        ⇒ updateExpressionBuffer(_.cursorRight)
      case BackwardChar       ⇒ updateExpressionBuffer(_.cursorLeft)
      case ForwardWord        ⇒ updateExpressionBuffer(_.forwardWord)
      case BackwardWord       ⇒ updateExpressionBuffer(_.backwardWord)
      case DeleteChar         ⇒ updateExpressionBuffer(_.delete)
      case BackwardDeleteChar ⇒ updateExpressionBuffer(_.backspace)
      case KillLine           ⇒ updateExpressionBuffer(_.deleteToEndOfLine)
      case BackwardKillLine   ⇒ updateExpressionBuffer(_.deleteToBeginningOfLine)
      case KillWord           ⇒ updateExpressionBuffer(_.deleteForwardWord)
      case BackwardKillWord   ⇒ updateExpressionBuffer(_.deleteBackwardWord)
      case ToggleQuote        ⇒ updateExpressionBuffer(QuoteToggler.toggleQuotes(_, mish = false))
      case Complete           ⇒
        for (result ← complete(expressionState.lineBuffer, mish = false)) {
          result.completions match {
            case Seq(completion) ⇒ immediateInsert(browserState, expressionState.lineBuffer, completion, result)
            case _               ⇒ enterIncrementalCompletionState(browserState, result, expressionState.lineBuffer)
          }
        }
      case AcceptLine         ⇒
        updateState(browserState.acceptExpression)
        acceptExpression(browserState.path, browserState.rawValue, expressionState.lineBuffer.text)
      case _                  ⇒
    }
  }

  private def enterIncrementalCompletionState(browserState: BrowserState, result: CompletionResult, lineBuffer: LineBuffer) = {
    val (completionState, newLineBuffer) = initialIncrementalCompletionState(result, lineBuffer)
    updateState(browserState.setExpression(ExpressionState(newLineBuffer, Some(completionState))))
  }

  private def immediateInsert(browserState: BrowserState, lineBuffer: LineBuffer, completion: Completion, result: CompletionResult) {
    val newText = result.replacementLocation.replace(lineBuffer.text, completion.replacement)
    val newOffset = result.replacementLocation.offset + completion.replacement.length
    updateState(browserState.setExpression(ExpressionState(LineBuffer(newText, newOffset))))
  }

  private def acceptExpression(currentPath: String, currentValue: MashValue, expression: String) =
    if (expression startsWith currentPath)
      acceptFollowOnExpression(currentPath, currentValue, expression drop currentPath.length)
    else
      acceptReplacementExpression(expression)

  private def acceptReplacementExpression(expression: String): Unit =
    for (result <- run(expression))
      focus(result, expression, tree = false)

  private def acceptFollowOnExpression(currentPath: String, currentValue: MashValue, followOnExpression: String): Unit = {
    val newPath = combineSafely(currentPath, followOnExpression)
    val expressionToEvaluate = combineSafely("it", followOnExpression)
    for (result <- run(expressionToEvaluate, Map(MashString("it") -> currentValue)))
      focus(result, newPath, tree = false)
  }

  private def run(expression: String, extraGlobals: Map[MashValue, MashValue] = Map()): Option[MashValue] = {
    val isolatedGlobals = MashObject.of(state.globalVariables.immutableFields ++ extraGlobals)
    val commandRunner = new CommandRunner(output, terminal.info, isolatedGlobals, sessionId, printErrors = false)
    val compilationUnit = CompilationUnit(expression)
    commandRunner.runCompilationUnit(compilationUnit, state.bareWords)
  }

  protected def handleOpenItem(browserState: BrowserState) = {
    state.lineBuffer = LineBuffer.Empty
    state.objectBrowserStateStackOpt = None
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
