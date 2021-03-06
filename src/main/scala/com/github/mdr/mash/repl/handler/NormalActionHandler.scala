package com.github.mdr.mash.repl.handler

import java.nio.file.Path

import com.github.mdr.mash.assist.InvocationAssistanceUpdater
import com.github.mdr.mash.commands.{ CommandResult, CommandRunner }
import com.github.mdr.mash.completions.{ Completion, CompletionResult }
import com.github.mdr.mash.editor.{ QuoteToggler, SyntaxSelection }
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.lexer.{ MashLexer, TokenType }
import com.github.mdr.mash.ns.view.ViewClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl.NormalActions.{ Down, _ }
import com.github.mdr.mash.repl.ReplVariables.{ It, ResultVarPrefix, ResultsListName }
import com.github.mdr.mash.repl.browser._
import com.github.mdr.mash.repl.{ LineBuffer, LineBufferActionHandler, Repl, UndoRedoState }
import com.github.mdr.mash.runtime.{ MashList, MashNull, MashValue }

import scala.util.control.NonFatal

trait NormalActionHandler extends InlineHandler {
  self: Repl ⇒

  private val fileSystem = LinuxFileSystem

  def handleNormalAction(action: InputAction) = {
    action match {
      case Enter                      ⇒ handleEnter()
      case BackwardKillWord           ⇒ handleBackwardKillWord()
      case LineBufferActionHandler(f) ⇒ handleTextChange(state = state.updateLineBuffer(f))
      case Complete                   ⇒ handleComplete()
      case RedrawScreen               ⇒ handleRedrawScreen()
      case EndOfFile                  ⇒ handleEof()
      case Up                         ⇒ handleUp()
      case UpExtendingSelection       ⇒ handleTextChange(state = state.updateLineBuffer(_.cursorUp(extendSelection = true)))
      case Down                       ⇒ handleDown()
      case DownExtendingSelection     ⇒ handleTextChange(state = state.updateLineBuffer(_.cursorDown(extendSelection = true)))
      case AssistInvocation           ⇒ handleAssistInvocation()
      case InsertLastArg              ⇒ handleInsertLastArg()
      case ToggleQuote                ⇒ handleToggleQuote()
      case ToggleMish                 ⇒ handleToggleMish()
      case IncrementalHistorySearch   ⇒ handleIncrementalHistorySearch()
      case BrowseLastResult           ⇒ handleBrowseLastResult()
      case Inline                     ⇒ handleInline()
      case ExpandSelection            ⇒ handleExpandSelection()
      case UnexpandSelection          ⇒ handleUnexpandSelection()
      case Copy                       ⇒ handleCopy()
      case Paste                      ⇒ handlePaste()
      case Undo                       ⇒ handleUndo()
      case Redo                       ⇒ handleRedo()
      case Quit                       ⇒ handleQuit()
      case _                          ⇒
    }
    if (action != Down && action != Up && action != RedrawScreen)
      history.commitToEntry()
    if (action != InsertLastArg && action != RedrawScreen)
      state = state.copy(insertLastArgStateOpt = None)
  }

  private def handleInline(): Unit = handleTextChange {
    state = handleInline(state)
  }

  private def handleBackwardKillWord() = handleTextChange {
    state = state.lineBuffer.selectedTextOpt
      .map(state.withCopied)
      .getOrElse(state)
      .updateLineBuffer(_.deleteBackwardWord)
  }

  private def handlePaste() = handleTextChange {
    for (copy ← state.copiedOpt)
      state = state.updateLineBuffer(_.insertAtCursor(copy))
  }

  private def handleCopy() = handleTextChange {
    state = state.lineBuffer.selectedTextOpt
      .map(state.withCopied)
      .getOrElse(state)
  }

  private def handleUndo() =
    for ((lineBuffer, newUndoRedoState) ← state.undoRedoState.undo(state.lineBuffer)) {
      state = state.withLineBuffer(lineBuffer).copy(undoRedoState = newUndoRedoState, oldSelections = Seq())
      history.resetHistoryPosition()
    }

  private def handleRedo() =
    for ((lineBuffer, newUndoRedoState) ← state.undoRedoState.redo(state.lineBuffer)) {
      state = state.withLineBuffer(lineBuffer).copy(undoRedoState = newUndoRedoState, oldSelections = Seq())
      history.resetHistoryPosition()
    }

  private def handleExpandSelection() = {
    for (newSelection ← SyntaxSelection.expandSelection(state.lineBuffer, state.mish))
      state = state.pushSelection(newSelection)
  }

  private def handleUnexpandSelection() = {
    state = state.popSelection
  }

  private def handleBrowseLastResult() {
    if (state.commandNumber > 0) {
      val commandNumber = state.commandNumber - 1
      val path = s"$ResultVarPrefix$commandNumber"
      for (value ← globalVariables.get(path)) {
        val browserState = getNewBrowserState(value, path)
        state = state.copy(objectBrowserStateStackOpt = Some(ObjectBrowserStateStack(List(browserState))))
      }
    }
  }

  private def handleIncrementalHistorySearch() = handleTextChange {
    state = IncrementalHistorySearchActionHandler(history, fileSystem, currentDirectoryManager).beginFreshIncrementalSearch(state)
  }

  private def handleTextChange[T](f: ⇒ T): T = {
    val oldLineBuffer = state.lineBuffer
    val result = f
    val newLineBuffer = state.lineBuffer
    if (oldLineBuffer.text != newLineBuffer.text) {
      history.resetHistoryPosition()
      state = state.copy(undoRedoState = state.undoRedoState.push(oldLineBuffer.withoutSelection))
    }
    if (oldLineBuffer != newLineBuffer) {
      state = state.copy(oldSelections = Seq())
    }
    result
  }

  private def handleUp() = {
    val lineBuffer = state.lineBuffer
    if (lineBuffer.isMultiline && history.isCommittedToEntry)
      handleTextChange(state = state.updateLineBuffer(_.cursorUp()))
    else {
      val shouldInitiateIncrementalSearch =
        !lineBuffer.isMultiline && !lineBuffer.text.trim.isEmpty && history.isCommittedToEntry
      if (shouldInitiateIncrementalSearch)
        handleTextChange(state = IncrementalHistorySearchActionHandler(history, fileSystem, currentDirectoryManager).beginIncrementalSearchFromLine(state))
      else
        history.goBackwards(lineBuffer.text) match {
          case Some(cmd) ⇒ state = state.withLineBuffer(LineBuffer(cmd))
          case None      ⇒ state = state.updateLineBuffer(_.cursorUp())
        }
    }
  }

  private def handleDown() =
    if (state.lineBuffer.isMultiline && history.isCommittedToEntry)
      handleTextChange(state = state.updateLineBuffer(_.cursorDown()))
    else
      history.goForwards() match {
        case Some(cmd) ⇒ state = state.withLineBuffer(LineBuffer(cmd))
        case None      ⇒ state = state.updateLineBuffer(_.cursorDown())
      }

  private def handleToggleQuote() = handleTextChange {
    state = state.updateLineBuffer(QuoteToggler.toggleQuotes(_, state.mish))
  }

  private def handleEof() {
    state = state.reset.copy(continue = false)
  }

  protected def handleRedrawScreen() {
    clearScreen()
    previousScreenOpt = None
  }

  private def handleToggleMish() = handleTextChange {
    val lineBuffer = state.lineBuffer
    val newLineBuffer =
      if (lineBuffer.text startsWith "!")
        state.lineBuffer.delete(0)
      else
        state.lineBuffer.insert("!", 0)
    state = state.withLineBuffer(newLineBuffer)
  }

  private def handleInsertLastArg() = handleTextChange {
    state = InsertLastArgHandler.handleInsertLastArg(history, state)
  }

  private def handleEnter() = {
    history.resetHistoryPosition()
    if (canAcceptBuffer)
      acceptLine()
    else
      handleTextChange(state = state.updateLineBuffer(_.insertAtCursor('\n')))
  }

  private def acceptLine() {
    updateScreenAfterFinishingWithLine()
    previousScreenOpt = None

    val cmd = state.lineBuffer.text
    state = state.withLineBuffer(LineBuffer.Empty).copy(undoRedoState = UndoRedoState.Clean, oldSelections = Seq())
    if (cmd.trim.nonEmpty)
      runCommand(cmd)
  }

  private def handleQuit() {
    if (state.assistanceStateOpt.isDefined)
      state = state.copy(assistanceStateOpt = None)
    else {
      state = state.withLineBuffer(LineBuffer.Empty)
      history.resetHistoryPosition()
      updateScreenAfterFinishingWithLine()
      previousScreenOpt = None
      state = state.withLineBuffer(LineBuffer.Empty).copy(undoRedoState = UndoRedoState.Clean, oldSelections = Seq())
    }
  }

  def canAcceptBuffer: Boolean = {
    val tokens = MashLexer.tokenise(state.lineBuffer.text, forgiving = true, mish = state.mish).tokens
    // TODO: We'll want to be smarter than this:
    import TokenType._
    val OpenBraceTypes: Set[TokenType] = Set(LBRACE, MISH_INTERPOLATION_START, MISH_INTERPOLATION_START_NO_CAPTURE,
      STRING_INTERPOLATION_START_COMPLEX)
    val openBraceCount = tokens.count(token ⇒ OpenBraceTypes.contains(token.tokenType))
    val mismatchedBrackets = openBraceCount != tokens.count(_.tokenType == TokenType.RBRACE)
    (state.lineBuffer.cursorAtEnd || !state.lineBuffer.isMultiline) && !mismatchedBrackets
  }

  protected def updateScreenAfterFinishingWithLine() {
    state = state.copy(
      completionStateOpt = None,
      assistanceStateOpt = None,
      lineBuffer = state.lineBuffer.moveCursorToEnd())
    draw(completed = true)

    for (previousScreen ← previousScreenOpt) {
      output.write(previousScreen.acceptScreen.getBytes)
      output.flush()
    }
  }

  protected def runCommand(cmd: String) {
    val commandRunner = new CommandRunner(output, terminal.size, globalVariables, sessionId)
    val unitName = s"command-${state.commandNumber}"
    val commandResult =
      try
        commandRunner.run(cmd, unitName, state.mish, bareWords, viewConfig)
      catch {
        case NonFatal(e) ⇒
          e.printStackTrace()
          debugLogger.logException(e)
          return
      }
    processCommandResult(cmd, commandResult, workingDirectory = fileSystem.pwd)
  }

  protected def runCommandQuietly(command: String) {
    val commandRunner = new CommandRunner(output, terminal.size, globalVariables, sessionId)
    val unitName = "quiet-command"
    try
      commandRunner.run(command, unitName, mish = false, bareWords, viewConfig)
    catch {
      case NonFatal(e) ⇒
        debugLogger.logException(e)
    }
  }

  private def processCommandResult(cmd: String, commandResult: CommandResult, workingDirectory: Path) {
    val CommandResult(valueOpt, toggleMish, displayModelOpt) = commandResult
    val actualResultOpt = valueOpt.map(ViewClass.unpackView)
    val commandNumber = state.commandNumber
    if (toggleMish)
      state = state.copy(mish = !state.mish)
    else {
      history.record(cmd, commandNumber, workingDirectory, state.mish, actualResultOpt)
      state = state.incrementCommandNumber
      for (result ← actualResultOpt)
        saveResult(commandNumber, result)
    }

    for (displayModel ← displayModelOpt) {
      val path = s"$ResultVarPrefix$commandNumber"
      val browserState = BrowserState.fromModel(displayModel, path)
      state = state.copy(objectBrowserStateStackOpt = Some(ObjectBrowserStateStack(List(browserState))))
    }
  }

  private def saveResult(commandNumber: Int, result: MashValue) {
    globalVariables.set(It, result)
    globalVariables.set(ResultVarPrefix + commandNumber, result)
    saveResultInList(result, commandNumber)
  }

  private def saveResultInList(result: MashValue, commandNumber: Int) {
    val oldResults =
      globalVariables.get(ResultsListName)
        .collect { case xs: MashList ⇒ xs.immutableElements }
        .getOrElse(Seq())
    val extendedResults = oldResults ++ Seq.fill(commandNumber - oldResults.length + 1)(MashNull)
    val newResults = MashList(extendedResults.updated(commandNumber, result))
    globalVariables.set(ResultsListName, newResults)
  }

  private def handleComplete() = handleTextChange {
    for (result ← complete) {
      history.resetHistoryPosition()
      result.completions match {
        case Seq(completion) ⇒ immediateInsert(completion, result)
        case _               ⇒ enterIncrementalCompletionState(result)
      }
    }
  }

  private def handleAssistInvocation() = handleTextChange {
    state = InvocationAssistanceUpdater.toggleInvocationAssistance(state, getBindings)
  }

  private def immediateInsert(completion: Completion, result: CompletionResult) {
    val newText = result.replacementLocation.replace(state.lineBuffer.text, completion.replacement)
    val newOffset = result.replacementLocation.offset + completion.replacement.length
    state = state.withLineBuffer(LineBuffer(newText, newOffset))
  }

}