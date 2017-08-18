package com.github.mdr.mash.repl.browser.handler

import com.github.mdr.mash.assist.InvocationAssistanceUpdater
import com.github.mdr.mash.commands.CommandRunner
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.completions.{ Completion, CompletionResult }
import com.github.mdr.mash.editor.{ QuoteToggler, SyntaxSelection }
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.parser.ExpressionCombiner._
import com.github.mdr.mash.repl.NormalActions.{ AssistInvocation, _ }
import com.github.mdr.mash.repl.browser.{ BrowserState, ExpressionState }
import com.github.mdr.mash.repl.completions.BrowseCompletionActions.{ AcceptCompletion, NavigateUp, _ }
import com.github.mdr.mash.repl.completions.{ BrowserCompletionState, IncrementalCompletionState, ReplStateMemento }
import com.github.mdr.mash.repl.{ LineBuffer, LineBufferActionHandler, Repl, ReplState }
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashValue }

trait ExpressionActionHandler {
  self: Repl ⇒

  protected def handleBrowserCompletionAction(action: InputAction,
                                              browserState: BrowserState,
                                              expressionState: ExpressionState,
                                              completionState: BrowserCompletionState) {
    val navigator = gridNavigator(completionState)
    def navigate(activeCompletion: Int) = {
      val (newCompletionState, newLineBuffer) = getBrowseCompletionState(completionState,
        activeCompletion = activeCompletion, expressionState.lineBuffer)
      val newExpressionState = expressionState.copy(completionStateOpt = Some(newCompletionState), lineBuffer = newLineBuffer)
      updateState(browserState.withExpressionState(newExpressionState))
    }
    action match {
      case NextCompletion     ⇒ navigate(navigator.next)
      case PreviousCompletion ⇒ navigate(navigator.previous)
      case NavigateRight      ⇒ navigate(navigator.right)
      case NavigateLeft       ⇒ navigate(navigator.left)
      case NavigateDown       ⇒ navigate(navigator.down)
      case NavigateUp         ⇒ navigate(navigator.up)
      case AcceptCompletion   ⇒
        val newExpressionState = expressionState.copy(completionStateOpt = None)
        updateState(browserState.withExpressionState(newExpressionState))
      case _                  ⇒
        val newExpressionState = expressionState.copy(completionStateOpt = None)
        val newBrowserState = browserState.withExpressionState(newExpressionState)
        updateState(newBrowserState)
        handleNormalExpressionInputAction(action, newBrowserState, newExpressionState)
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
        updateState(browserState.withExpressionState(newExpressionState))
      case BackwardDeleteChar if completionState.mementoOpt.isDefined ⇒ // restore previous state
        for (ReplStateMemento(lineBuffer, completionState) ← completionState.mementoOpt) {
          val newExpressionState = expressionState.copy(completionStateOpt = Some(completionState), lineBuffer = lineBuffer)
          updateState(browserState.withExpressionState(newExpressionState))
        }
      case Complete if completionState.immediatelyAfterCompletion     ⇒ // enter browse completions mode
        val (newCompletionState, newLineBuffer) = getBrowseCompletionState(completionState, activeCompletion = 0, expressionState.lineBuffer)
        val newExpressionState = expressionState.copy(completionStateOpt = Some(newCompletionState), lineBuffer = newLineBuffer)
        updateState(browserState.withExpressionState(newExpressionState))
      case _                                                          ⇒ // exit back to normal mode, and handle there
        val newExpressionState = expressionState.copy(completionStateOpt = None)
        val newBrowserState = browserState.withExpressionState(newExpressionState)
        updateState(newBrowserState)
        handleNormalExpressionInputAction(action, newBrowserState, newExpressionState)
    }

  protected def handleBrowserExpressionInputAction(action: InputAction, browserState: BrowserState, expressionState: ExpressionState) =
    expressionState.completionStateOpt match {
      case Some(completionState: IncrementalCompletionState) ⇒
        handleIncrementalCompletionAction(action, browserState, expressionState, completionState)
      case Some(completionState: BrowserCompletionState)     ⇒
        handleBrowserCompletionAction(action, browserState, expressionState, completionState)
      case _                                                 ⇒
        handleNormalExpressionInputAction(action, browserState, expressionState)
    }

  private def handleNormalExpressionInputAction(action: InputAction, browserState: BrowserState, expressionState: ExpressionState) {
    def updateExpressionBuffer(f: LineBuffer ⇒ LineBuffer) =
      updateState(browserState.withExpressionState(expressionState.updateLineBuffer(f)))
    val context = Context(browserState, expressionState)
    action match {
      case Enter                      ⇒ handleEnter(browserState, expressionState)
      case BackwardKillWord           ⇒ handleBackwardKillWord(browserState, expressionState)
      case LineBufferActionHandler(f) ⇒ updateExpressionBuffer(f)
      case Complete                   ⇒ handleComplete(browserState, expressionState)
      case ToggleQuote                ⇒ updateExpressionBuffer(QuoteToggler.toggleQuotes(_, mish = false))
      case ExpandSelection            ⇒ handleExpandSelection(browserState, expressionState)
      case UnexpandSelection          ⇒ handleUnexpandSelection(browserState, expressionState)
      case AssistInvocation           ⇒ handleAssistInvocation(browserState, expressionState)
      case Copy                       ⇒ handleCopy(context)
      case Paste                      ⇒ handlePaste(context)
      case Quit                       ⇒ handleQuit(browserState)
      case Inline                     ⇒ updateExpressionBuffer(lineBuffer ⇒ handleInline(lineBuffer, mish = false))
      case _                          ⇒
    }
    updateInvocationAssistance(browserState)
  }

  private def handleQuit(browserState: BrowserState) =
    updateState(browserState.acceptExpression)

  private def handleExpandSelection(browserState: BrowserState, expressionState: ExpressionState) =
    for (newSelection ← SyntaxSelection.expandSelection(expressionState.lineBuffer))
      updateState(browserState.withExpressionState(expressionState.pushSelection(newSelection)))

  private def handleUnexpandSelection(browserState: BrowserState, expressionState: ExpressionState) =
    updateState(browserState.withExpressionState(expressionState.popSelection))

  private def handleBackwardKillWord(browserState: BrowserState, expressionState: ExpressionState) = {
    val newExpressionState =
      expressionState.lineBuffer.selectedTextOpt
        .map(expressionState.withCopied)
        .getOrElse(expressionState)
        .updateLineBuffer(_.deleteBackwardWord)
    updateState(browserState.withExpressionState(newExpressionState))
  }

  private def updateState(context: Context): Unit = updateState(context.browserState)

  private case class Context(browserState: BrowserState, expressionState: ExpressionState) {

    def lineBuffer: LineBuffer = expressionState.lineBuffer

    def withLineBuffer(lineBuffer: LineBuffer): Context =
      updateExpressionState(_.withLineBuffer(lineBuffer))

    def updateLineBuffer(f: LineBuffer ⇒ LineBuffer): Context =
      withLineBuffer(f(expressionState.lineBuffer))

    def withExpressionState(newExpressionState: ExpressionState): Context = {
      val newBrowserState = browserState.withExpressionState(newExpressionState)
      Context(newBrowserState, newExpressionState)
    }

    def updateExpressionState(f: ExpressionState ⇒ ExpressionState): Context =
      withExpressionState(f(expressionState))

  }

  private def handlePaste(context: Context) = {
    for (text ← context.expressionState.copiedOpt)
      updateState(context.updateLineBuffer(_.insertAtCursor(text)))
  }

  private def handleCopy(context: Context) =
    updateState(
      context.lineBuffer.selectedTextOpt
        .map(text ⇒ context.updateExpressionState(_.withCopied(text)))
        .getOrElse(context))

  private def handleAssistInvocation(browserState: BrowserState, expressionState: ExpressionState) {
    val newExpressionState = toggleInvocationAssistance(expressionState)
    updateState(browserState.withExpressionState(newExpressionState))
  }

  private def handleEnter(browserState: BrowserState, expressionState: ExpressionState) {
    updateState(browserState.acceptExpression)
    acceptExpression(browserState.path, browserState.rawValue, expressionState.lineBuffer.text)
  }

  private def handleComplete(browserState: BrowserState, expressionState: ExpressionState) =
    for (result ← complete(expressionState.lineBuffer, mish = false))
      result.completions match {
        case Seq(completion) ⇒ immediateInsert(browserState, expressionState.lineBuffer, completion, result)
        case _               ⇒ enterIncrementalCompletionState(browserState, result, expressionState.lineBuffer)
      }

  private def updateInvocationAssistance(browserState: BrowserState): Unit = {
    for {
      browserStateStack ← state.objectBrowserStateStackOpt
      expressionState ← browserStateStack.headState.expressionStateOpt
      assistanceState ← expressionState.assistanceStateOpt
    } {
      val newAssistanceStateOpt = InvocationAssistanceUpdater.updateInvocationAssistance(expressionState.lineBuffer,
        getBindings, mish = false, Some(assistanceState))
      val newExpressionState = expressionState.copy(assistanceStateOpt = newAssistanceStateOpt)
      updateState(browserState.withExpressionState(newExpressionState))
    }
  }

  private def toggleInvocationAssistance(expressionState: ExpressionState): ExpressionState =
    if (expressionState.assistanceStateOpt.isDefined)
      expressionState.copy(assistanceStateOpt = None)
    else {
      val newAssistanceStateOpt = InvocationAssistanceUpdater.updateInvocationAssistance(expressionState.lineBuffer,
        getBindings, mish = false, expressionState.assistanceStateOpt)
      expressionState.copy(assistanceStateOpt = newAssistanceStateOpt)
    }

  private def enterIncrementalCompletionState(browserState: BrowserState, result: CompletionResult, lineBuffer: LineBuffer) = {
    val (completionState, newLineBuffer) = initialIncrementalCompletionState(result, lineBuffer)
    updateState(browserState.withExpressionState(ExpressionState(newLineBuffer, completionStateOpt = Some(completionState))))
  }

  private def immediateInsert(browserState: BrowserState, lineBuffer: LineBuffer, completion: Completion, result: CompletionResult) {
    val newText = result.replacementLocation.replace(lineBuffer.text, completion.replacement)
    val newOffset = result.replacementLocation.offset + completion.replacement.length
    updateState(browserState.withExpressionState(ExpressionState(LineBuffer(newText, newOffset))))
  }

  private def acceptExpression(currentPath: String, currentValue: MashValue, expression: String) =
    if (expression startsWith currentPath)
      acceptFollowOnExpression(currentPath, currentValue, expression drop currentPath.length)
    else
      acceptReplacementExpression(expression)

  protected def acceptReplacementExpression(expression: String): Unit =
    for (result ← run(expression))
      focus(result, expression, tree = false)

  protected def acceptFollowOnExpression(currentPath: String, currentValue: MashValue, followOnExpression: String) {
    val newPath = combineSafely(currentPath, followOnExpression)
    val expressionToEvaluate = combineSafely("it", followOnExpression)
    for (result ← run(expressionToEvaluate, Map(MashString("it") -> currentValue)))
      focus(result, newPath, tree = false)
  }

  private def run(expression: String, extraGlobals: Map[MashValue, MashValue] = Map()): Option[MashValue] = {
    val isolatedGlobals = MashObject.of(globalVariables.immutableFields ++ extraGlobals)
    val commandRunner = new CommandRunner(output, terminal.size, isolatedGlobals, sessionId, printErrors = false)
    val compilationUnit = CompilationUnit(expression)
    commandRunner.runCompilationUnit(compilationUnit, bareWords)
  }

}
