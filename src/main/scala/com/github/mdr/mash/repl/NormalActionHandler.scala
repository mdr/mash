package com.github.mdr.mash.repl

import com.github.mdr.mash.CommandResult
import com.github.mdr.mash.CommandRunner
import com.github.mdr.mash.DebugLogger
import com.github.mdr.mash.completions.Completion
import com.github.mdr.mash.completions.CompletionResult
import com.github.mdr.mash.editor.QuoteToggler
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.incrementalSearch.IncrementalSearchState
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.ns.view.ViewClass
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.terminal.Terminal
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashValue

trait NormalActionHandler { self: Repl ⇒

  def handleNormalAction(action: InputAction) = {
    action match {
      case AcceptLine               ⇒ handleAcceptLine()
      case Complete                 ⇒ handleComplete()
      case ClearScreen              ⇒ handleClearScreen()
      case EndOfFile                ⇒ handleEof()
      case PreviousHistory          ⇒ for (cmd ← history.goBackwards()) state.lineBuffer = LineBuffer(cmd)
      case NextHistory              ⇒ for (cmd ← history.goForwards()) state.lineBuffer = LineBuffer(cmd)
      case BeginningOfLine          ⇒ state.updateLineBuffer(_.moveCursorToStart)
      case EndOfLine                ⇒ state.updateLineBuffer(_.moveCursorToEnd)
      case ForwardChar              ⇒ state.updateLineBuffer(_.cursorRight)
      case BackwardChar             ⇒ state.updateLineBuffer(_.cursorLeft)
      case ForwardWord              ⇒ state.updateLineBuffer(_.forwardWord)
      case BackwardWord             ⇒ state.updateLineBuffer(_.backwardWord)
      case DeleteChar               ⇒ state.updateLineBuffer(_.delete)
      case BackwardDeleteChar       ⇒ state.updateLineBuffer(_.backspace)
      case KillLine                 ⇒ state.updateLineBuffer(_.deleteToEndOfLine)
      case KillWord                 ⇒ state.updateLineBuffer(_.deleteForwardWord)
      case BackwardKillWord         ⇒ state.updateLineBuffer(_.deleteBackwardWord)
      case SelfInsert(s)            ⇒ for (c ← s) state.updateLineBuffer(_.addCharacterAtCursor(c))
      case AssistInvocation         ⇒ handleAssistInvocation()
      case IncrementalHistorySearch ⇒ state.incrementalSearchStateOpt = Some(IncrementalSearchState())
      case YankLastArg              ⇒ handleYankLastArg()
      case ToggleQuote              ⇒ handleToggleQuote()
      case ToggleMish               ⇒ handleToggleMish()
      case _                        ⇒
    }
    if (action != YankLastArg && action != ClearScreen)
      state.yankLastArgStateOpt = None
  }

  private def handleToggleQuote() {
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

  private def handleToggleMish() {
    state.lineBuffer =
      if (state.lineBuffer.text startsWith "!")
        state.lineBuffer.delete(0)
      else
        state.lineBuffer.insertCharacters("!", 0)
  }

  private def handleYankLastArg() {
    val (argIndex, oldRegion) = state.yankLastArgStateOpt match {
      case Some(YankLastArgState(n, region)) ⇒ (n + 1, region)
      case None                              ⇒ (0, Region(state.lineBuffer.cursorPos, 0))
    }
    history.getLastArg(argIndex) match {
      case Some(newArg) ⇒
        val newS = oldRegion.replace(state.lineBuffer.text, newArg)
        val newRegion = Region(oldRegion.offset, newArg.length)
        state.lineBuffer = LineBuffer(newS, newRegion.posAfter)
        state.yankLastArgStateOpt = Some(YankLastArgState(argIndex, newRegion))
      case None ⇒
    }
  }

  private def handleAcceptLine() {
    updateScreenAfterAccept()
    previousReplRenderResultOpt = None

    history.resetHistoryPosition()

    val cmd = state.lineBuffer.text
    state.lineBuffer = LineBuffer.Empty
    if (cmd.trim.nonEmpty)
      runCommand(cmd)
  }

  private def updateScreenAfterAccept() {
    state.completionStateOpt = None
    state.assistanceStateOpt = None
    draw()

    for (renderResult ← previousReplRenderResultOpt) {
      output.write(renderResult.screen.acceptScreen.getBytes)
      output.flush()
    }
  }

  private def runCommand(cmd: String) {
    val commandRunner = new CommandRunner(output, terminal.info, getEnvironment)
    val commandResult =
      try
        commandRunner.run(cmd, state.mish, state.bareWords)
      catch {
        case e: Exception ⇒
          e.printStackTrace()
          DebugLogger.logException(e)
          return
      }
    processCommandResult(cmd, commandResult)
  }

  private def processCommandResult(cmd: String, commandResult: CommandResult) {
    val CommandResult(resultOpt, toggleMish, objectTableModelOpt) = commandResult
    val actualResultOpt = resultOpt.map {
      case obj @ MashObject(_, Some(ViewClass)) ⇒ obj.getField(ViewClass.Fields.Data).getOrElse(obj)
      case result                               ⇒ result
    }
    val commandNumber = state.commandNumber
    if (toggleMish)
      state.mish = !state.mish
    else {
      history.record(cmd, commandNumber, state.mish, actualResultOpt)
      state.commandNumber += 1
    }
    actualResultOpt.foreach(saveResult(commandNumber))

    for (objectTableModel ← objectTableModelOpt)
      state.objectBrowserStateOpt = Some(ObjectBrowserState(objectTableModel))
  }

  private def saveResult(number: Int)(result: MashValue) {
    state.globalVariables += ReplState.It -> result
    val oldResults = state.globalVariables.get(ReplState.Res) match {
      case Some(MashList(oldResults @ _*)) ⇒ oldResults
      case _                               ⇒ Seq()
    }
    val extendedResults = oldResults ++ Seq.fill(number - oldResults.length + 1)(MashNull)
    val newResults = MashList(extendedResults.updated(number, result))
    state.globalVariables += ReplState.Res -> newResults
  }

  private def handleComplete() =
    for (result ← complete)
      result.completions match {
        case Seq(completion) ⇒ immediateInsert(completion, result)
        case _               ⇒ enterIncrementalCompletionState(result)
      }

  private def handleAssistInvocation() {
    if (state.assistanceStateOpt.isDefined)
      state.assistanceStateOpt = None
    else
      updateInvocationAssistance()
  }

  private def immediateInsert(completion: Completion, result: CompletionResult) {
    val newText = result.replacementLocation.replace(state.lineBuffer.text, completion.replacement)
    val newCursorPos = result.replacementLocation.offset + completion.replacement.length
    state.lineBuffer = LineBuffer(newText, newCursorPos)
  }

}