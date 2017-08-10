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
import com.github.mdr.mash.repl.{ LineBuffer, LineBufferActionHandler, Repl }
import com.github.mdr.mash.runtime.{ MashList, MashNull, MashObject, MashValue }
import com.github.mdr.mash.terminal.Terminal

import scala.PartialFunction.cond
import scala.util.control.NonFatal

trait NormalActionHandler extends InlineHandler {
  self: Repl ⇒

  private val fileSystem = LinuxFileSystem

  def handleNormalAction(action: InputAction) = {
    action match {
      case Enter                      ⇒ handleEnter()
      case LineBufferActionHandler(f) ⇒
        resetHistoryIfTextChanges(state = state.updateLineBufferResult(f))
      case Complete                   ⇒ handleComplete()
      case RedrawScreen               ⇒ handleRedrawScreen()
      case EndOfFile                  ⇒ handleEof()
      case Up                         ⇒ handleUp()
      case Down                       ⇒ handleDown()
      case AssistInvocation           ⇒ handleAssistInvocation()
      case InsertLastArg              ⇒ handleInsertLastArg()
      case ToggleQuote                ⇒ handleToggleQuote()
      case ToggleMish                 ⇒ handleToggleMish()
      case IncrementalHistorySearch   ⇒ handleIncrementalHistorySearch()
      case BrowseLastResult           ⇒ handleBrowseLastResult()
      case Inline                     ⇒ resetHistoryIfTextChanges(state = handleInline(state))
      case ExpandSelection            ⇒ handleExpandSelection()
      case Paste                      ⇒ handlePaste()
      case _                          ⇒
    }
    if (action != Down && action != Up && action != RedrawScreen)
      history.commitToEntry()
    if (action != InsertLastArg && action != RedrawScreen)
      state = state.copy(insertLastArgStateOpt = None)
  }

  private def handlePaste() = resetHistoryIfTextChanges {
    for (copy ← state.copiedOpt)
      state = state.updateLineBuffer(_.insertAtCursor(copy))
  }

  private def handleExpandSelection() =
    state = state.updateLineBuffer(SyntaxSelection.expandSelection(_, state.mish))

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

  private def handleIncrementalHistorySearch() =
    state = IncrementalHistorySearchActionHandler(history).beginFreshIncrementalSearch(state)

  private def resetHistoryIfTextChanges[T](f: ⇒ T): T = {
    val before = state.lineBuffer.text
    val result = f
    val after = state.lineBuffer.text
    if (before != after)
      history.resetHistoryPosition()
    result
  }

  private def handleUp() = {
    val lineBuffer = state.lineBuffer
    if (!lineBuffer.onFirstLine && history.isCommittedToEntry)
      state = state.updateLineBuffer(_.cursorUp)
    else {
      val shouldInitiateIncrementalSearch =
        !lineBuffer.isMultiline && !lineBuffer.text.trim.isEmpty && history.isCommittedToEntry
      if (shouldInitiateIncrementalSearch)
        state = IncrementalHistorySearchActionHandler(history).beginIncrementalSearchFromLine(state)
      else
        history.goBackwards(lineBuffer.text) match {
          case Some(cmd) ⇒ state = state.withLineBuffer(LineBuffer(cmd))
          case None      ⇒ state = state.updateLineBuffer(_.cursorUp)
        }
    }
  }

  private def handleDown() =
    if (state.lineBuffer.onLastLine || !history.isCommittedToEntry)
      history.goForwards() match {
        case Some(cmd) ⇒ state = state.withLineBuffer(LineBuffer(cmd))
        case None      ⇒ state = state.updateLineBuffer(_.cursorDown)
      }
    else
      state = state.updateLineBuffer(_.cursorDown)

  private def handleToggleQuote() = resetHistoryIfTextChanges {
    state = state.updateLineBuffer(QuoteToggler.toggleQuotes(_, state.mish))
  }

  private def handleEof() {
    state = state.reset.copy(continue = false)
  }

  protected def handleRedrawScreen() {
    output.write(Terminal.ClearScreenEscapeSequence.getBytes)
    output.flush()
    previousScreenOpt = None
  }

  private def handleToggleMish() = resetHistoryIfTextChanges {
    val lineBuffer = state.lineBuffer
    val newLineBuffer =
      if (lineBuffer.text startsWith "!")
        state.lineBuffer.delete(0)
      else
        state.lineBuffer.insert("!", 0)
    state = state.withLineBuffer(newLineBuffer)
  }

  private def handleInsertLastArg() = resetHistoryIfTextChanges {
    state = InsertLastArgHandler.handleInsertLastArg(history, state)
  }

  private def handleEnter() = {
    history.resetHistoryPosition()
    if (canAcceptBuffer) {
      updateScreenAfterAccept()
      previousScreenOpt = None

      val cmd = state.lineBuffer.text
      state = state.withLineBuffer(LineBuffer.Empty)
      if (cmd.trim.nonEmpty)
        runCommand(cmd)
    } else
      state = state.updateLineBuffer(_.insertAtCursor('\n'))
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

  protected def updateScreenAfterAccept() {
    state = state.copy(
      completionStateOpt = None,
      assistanceStateOpt = None,
      lineBuffer = state.lineBuffer.moveCursorToEnd)
    draw()

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

  private def processCommandResult(cmd: String, commandResult: CommandResult, workingDirectory: Path) {
    val CommandResult(valueOpt, toggleMish, displayModelOpt) = commandResult
    val actualResultOpt = valueOpt.map(ViewClass.unpackView)
    val commandNumber = state.commandNumber
    if (toggleMish)
      state = state.copy(mish = !state.mish)
    else {
      history.record(cmd, commandNumber, state.mish, actualResultOpt, workingDirectory)
      state = state.incrementCommandNumber
    }
    actualResultOpt.foreach(saveResult(commandNumber))

    for (displayModel ← displayModelOpt) {
      val isView = valueOpt.exists(cond(_) { case MashObject(_, Some(ViewClass)) ⇒ true })
      val path = if (isView) s"$ResultVarPrefix$commandNumber" else cmd
      val browserState = BrowserState.fromModel(displayModel, path)
      state = state.copy(objectBrowserStateStackOpt = Some(ObjectBrowserStateStack(List(browserState))))
    }
  }

  private def saveResult(commandNumber: Int)(result: MashValue) {
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

  private def handleComplete() =
    for (result ← complete) {
      history.resetHistoryPosition()
      result.completions match {
        case Seq(completion) ⇒ immediateInsert(completion, result)
        case _               ⇒ enterIncrementalCompletionState(result)
      }
    }

  private def handleAssistInvocation() =
    state = InvocationAssistanceUpdater.toggleInvocationAssistance(state, getBindings)

  private def immediateInsert(completion: Completion, result: CompletionResult) {
    val newText = result.replacementLocation.replace(state.lineBuffer.text, completion.replacement)
    val newOffset = result.replacementLocation.offset + completion.replacement.length
    state = state.withLineBuffer(LineBuffer(newText, newOffset))
  }

}