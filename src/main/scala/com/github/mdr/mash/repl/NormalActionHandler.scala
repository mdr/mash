package com.github.mdr.mash.repl

import java.nio.file.Path

import com.github.mdr.mash.commands.{ CommandResult, CommandRunner }
import com.github.mdr.mash.completions.{ Completion, CompletionResult }
import com.github.mdr.mash.editor.QuoteToggler
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.lexer.{ MashLexer, TokenType }
import com.github.mdr.mash.ns.view.ViewClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl.NormalActions.{ NextHistory, _ }
import com.github.mdr.mash.repl.browser._
import com.github.mdr.mash.repl.history.HistorySearchState
import com.github.mdr.mash.runtime.{ MashList, MashNull, MashObject, MashValue }
import com.github.mdr.mash.terminal.Terminal
import com.github.mdr.mash.utils.Region

import scala.PartialFunction.cond

trait NormalActionHandler {
  self: Repl ⇒
  private val fileSystem = LinuxFileSystem

  def handleNormalAction(action: InputAction) = {
    action match {
      case AcceptLine               ⇒ handleAcceptLine()
      case Complete                 ⇒ handleComplete()
      case ClearScreen              ⇒ handleClearScreen()
      case EndOfFile                ⇒ handleEof()
      case PreviousHistory          ⇒ handlePreviousHistory()
      case NextHistory              ⇒ handleNextHistory()
      case BeginningOfLine          ⇒ state.updateLineBuffer(_.moveCursorToStart)
      case EndOfLine                ⇒ state.updateLineBuffer(_.moveCursorToEnd)
      case ForwardChar              ⇒ state.updateLineBuffer(_.cursorRight)
      case BackwardChar             ⇒ state.updateLineBuffer(_.cursorLeft)
      case ForwardWord              ⇒ state.updateLineBuffer(_.forwardWord)
      case BackwardWord             ⇒ state.updateLineBuffer(_.backwardWord)
      case DeleteChar               ⇒ resetHistoryIfTextChanges {
        state.updateLineBuffer(_.delete)
      }
      case BackwardDeleteChar       ⇒ resetHistoryIfTextChanges {
        state.updateLineBuffer(_.backspace)
      }
      case KillLine                 ⇒ resetHistoryIfTextChanges {
        state.updateLineBuffer(_.deleteToEndOfLine)
      }
      case BackwardKillLine         ⇒ resetHistoryIfTextChanges {
        state.updateLineBuffer(_.deleteToBeginningOfLine)
      }
      case KillWord                 ⇒ resetHistoryIfTextChanges {
        state.updateLineBuffer(_.deleteForwardWord)
      }
      case BackwardKillWord         ⇒ resetHistoryIfTextChanges {
        state.updateLineBuffer(_.deleteBackwardWord)
      }
      case SelfInsert(s)            ⇒ handleSelfInsert(s)
      case AssistInvocation         ⇒ handleAssistInvocation()
      case InsertLastArg            ⇒ handleInsertLastArg()
      case ToggleQuote              ⇒ handleToggleQuote()
      case ToggleMish               ⇒ handleToggleMish()
      case IncrementalHistorySearch ⇒ handleIncrementalHistorySearch()
      case BrowseLastResult         ⇒ handleBrowseLastResult()
      case _                        ⇒
    }
    if (action != NextHistory && action != PreviousHistory && action != ClearScreen)
      history.commitToEntry()
    if (action != InsertLastArg && action != ClearScreen)
      state.insertLastArgStateOpt = None
  }

  private def handleBrowseLastResult() {
    if (state.commandNumber > 0) {
      val commandNumber = state.commandNumber - 1
      val path = s"${ReplState.ResultsListName}$commandNumber"
      for (value <- state.globalVariables.get(path)) {
        val browserState = getNewBrowserState(value, path)
        state.objectBrowserStateStackOpt = Some(ObjectBrowserStateStack(List(browserState)))
      }
    }
  }

  private def handleSelfInsert(s: String) =
    resetHistoryIfTextChanges {
      for (c ← s) state.updateLineBuffer(_.addCharacterAtCursor(c))
    }

  private def handleIncrementalHistorySearch() {
    state.assistanceStateOpt = None
    state.historySearchStateOpt = Some(HistorySearchState())
    history.resetHistoryPosition()
  }

  private def resetHistoryIfTextChanges[T](f: ⇒ T): T = {
    val before = state.lineBuffer.text
    val result = f
    val after = state.lineBuffer.text
    if (before != after)
      history.resetHistoryPosition()
    result
  }

  private def handlePreviousHistory() =
    if (state.lineBuffer.onFirstLine || !history.isCommittedToEntry)
      history.goBackwards(state.lineBuffer.text) match {
        case Some(cmd) ⇒ state.lineBuffer = LineBuffer(cmd)
        case None      ⇒ state.updateLineBuffer(_.up)
      }
    else
      state.updateLineBuffer(_.up)

  private def handleNextHistory() =
    if (state.lineBuffer.onLastLine || !history.isCommittedToEntry)
      history.goForwards() match {
        case Some(cmd) ⇒ state.lineBuffer = LineBuffer(cmd)
        case None      ⇒ state.updateLineBuffer(_.down)
      }
    else
      state.updateLineBuffer(_.down)

  private def handleToggleQuote(): Unit = resetHistoryIfTextChanges {
    state.updateLineBuffer(QuoteToggler.toggleQuotes(_, state.mish))
  }

  private def handleEof() {
    state.reset()
    state.continue = false
  }

  private def handleClearScreen() {
    output.write(Terminal.ClearScreenEscapeSequence.getBytes)
    output.flush()
    previousReplRenderResultOpt = None
  }

  private def handleToggleMish(): Unit = resetHistoryIfTextChanges {
    state.lineBuffer =
      if (state.lineBuffer.text startsWith "!")
        state.lineBuffer.delete(0)
      else
        state.lineBuffer.insertCharacters("!", 0)
  }

  private def handleInsertLastArg(): Unit = resetHistoryIfTextChanges {
    val (argIndex, oldRegion) = state.insertLastArgStateOpt match {
      case Some(InsertLastArgState(n, region)) ⇒ (n + 1, region)
      case None                                ⇒ (0, Region(state.lineBuffer.cursorOffset, 0))
    }
    history.getLastArg(argIndex) match {
      case Some(newArg) ⇒
        val newText = oldRegion.replace(state.lineBuffer.text, newArg)
        val newRegion = Region(oldRegion.offset, newArg.length)
        state.lineBuffer = LineBuffer(newText, newRegion.posAfter)
        state.insertLastArgStateOpt = Some(InsertLastArgState(argIndex, newRegion))
      case None         ⇒
    }
  }

  private def handleAcceptLine() =
    if (canAcceptBuffer) {
      updateScreenAfterAccept()
      previousReplRenderResultOpt = None

      history.resetHistoryPosition()

      val cmd = state.lineBuffer.text
      state.lineBuffer = LineBuffer.Empty
      if (cmd.trim.nonEmpty)
        runCommand(cmd)
    } else
      handleSelfInsert("\n")

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
    state.completionStateOpt = None
    state.assistanceStateOpt = None
    state.updateLineBuffer(_.moveCursorToEndOfLine) // To make sure we don't highlight any matching brackets
    draw()

    for (renderResult ← previousReplRenderResultOpt) {
      output.write(renderResult.screen.acceptScreen.getBytes)
      output.flush()
    }
  }

  protected def runCommand(cmd: String) {
    val commandRunner = new CommandRunner(output, terminal.info, state.globalVariables, sessionId)
    val unitName = s"command-${state.commandNumber}"
    val commandResult =
      try
        commandRunner.run(cmd, unitName, state.mish, state.bareWords, state.viewConfig)
      catch {
        case e: Exception ⇒
          e.printStackTrace()
          debugLogger.logException(e)
          return
      }
    processCommandResult(cmd, commandResult, workingDirectory = fileSystem.pwd)
  }

  private def unpackView(value: MashValue): MashValue = value match {
    case obj@MashObject(_, Some(ViewClass)) ⇒ obj.get(ViewClass.Fields.Data).map(unpackView).getOrElse(obj)
    case result                             ⇒ value
  }

  private def processCommandResult(cmd: String, commandResult: CommandResult, workingDirectory: Path) {
    val CommandResult(resultOpt, toggleMish, displayModelOpt) = commandResult
    val actualResultOpt = resultOpt.map(unpackView)
    val commandNumber = state.commandNumber
    if (toggleMish)
      state.mish = !state.mish
    else {
      history.record(cmd, commandNumber, state.mish, actualResultOpt, workingDirectory)
      state.commandNumber += 1
    }
    actualResultOpt.foreach(saveResult(commandNumber))

    for (displayModel ← displayModelOpt) {
      val isView = resultOpt.exists(cond(_) { case MashObject(_, Some(ViewClass)) ⇒ true })
      val path = if (isView) s"${ReplState.ResultsListName}$commandNumber" else cmd
      val browserState = BrowserState.fromModel(displayModel, path)
      state.objectBrowserStateStackOpt = Some(ObjectBrowserStateStack(List(browserState)))
    }
  }

  private def saveResult(number: Int)(result: MashValue) {
    state.globalVariables.set(ReplState.It, result)
    state.globalVariables.set(ReplState.ResultsListName + number, result)
    val oldResults = state.globalVariables.get(ReplState.ResultsListName) match {
      case Some(MashList(oldResults@_*)) ⇒ oldResults
      case _                             ⇒ Seq()
    }
    val extendedResults = oldResults ++ Seq.fill(number - oldResults.length + 1)(MashNull)
    val newResults = MashList(extendedResults.updated(number, result))
    state.globalVariables.set(ReplState.ResultsListName, newResults)
  }

  private def handleComplete() = {
    for (result ← complete) {
      history.resetHistoryPosition()
      result.completions match {
        case Seq(completion) ⇒ immediateInsert(completion, result)
        case _               ⇒ enterIncrementalCompletionState(result)
      }
    }
  }

  private def handleAssistInvocation() {
    if (state.assistanceStateOpt.isDefined)
      state.assistanceStateOpt = None
    else
      updateInvocationAssistance()
  }

  private def immediateInsert(completion: Completion, result: CompletionResult) {
    val newText = result.replacementLocation.replace(state.lineBuffer.text, completion.replacement)
    val newOffset = result.replacementLocation.offset + completion.replacement.length
    state.lineBuffer = LineBuffer(newText, newOffset)
  }

}