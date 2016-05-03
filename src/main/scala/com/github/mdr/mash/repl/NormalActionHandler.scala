package com.github.mdr.mash.repl

import com.github.mdr.mash.CommandResult
import com.github.mdr.mash.CommandRunner
import com.github.mdr.mash.DebugLogger
import com.github.mdr.mash.completions.CompletionResult
import com.github.mdr.mash.editor.QuoteToggler
import com.github.mdr.mash.evaluator.MashList
import com.github.mdr.mash.incrementalSearch.IncrementalSearchState
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.completions.Completion

object NormalActionHandler {

  private val ClearScreenEscapeSequence = "\u001b[H\u001b[2J"

}

trait NormalActionHandler { self: Repl ⇒
  import NormalActionHandler._
  import InputAction._

  def handleNormalAction(action: InputAction) = {
    action match {
      case AcceptLine ⇒
        handleAcceptLine()
      case Complete ⇒
        handleComplete()
      case ClearScreen ⇒
        System.out.write(ClearScreenEscapeSequence.getBytes)
        System.out.flush()
        previousReplRenderResultOpt = None
      case EndOfFile ⇒
        state.lineBuffer = LineBuffer.Empty
        state.completionStateOpt = None
        state.continue = false
        state.assistanceStateOpt = None
      case PreviousHistory    ⇒ for (cmd ← state.history.goBackwards()) state.lineBuffer = LineBuffer(cmd)
      case NextHistory        ⇒ for (cmd ← state.history.goForwards()) state.lineBuffer = LineBuffer(cmd)
      case BeginningOfLine    ⇒ state.updateLineBuffer(_.moveCursorToStart)
      case EndOfLine          ⇒ state.updateLineBuffer(_.moveCursorToEnd)
      case ForwardChar        ⇒ state.updateLineBuffer(_.cursorRight)
      case BackwardChar       ⇒ state.updateLineBuffer(_.cursorLeft)
      case ForwardWord        ⇒ state.updateLineBuffer(_.forwardWord)
      case BackwardWord       ⇒ state.updateLineBuffer(_.backwardWord)
      case DeleteChar         ⇒ state.updateLineBuffer(_.delete)
      case BackwardDeleteChar ⇒ state.updateLineBuffer(_.backspace)
      case KillLine           ⇒ state.updateLineBuffer(_.deleteToEndOfLine)
      case KillWord           ⇒ state.updateLineBuffer(_.deleteForwardWord)
      case BackwardKillWord   ⇒ state.updateLineBuffer(_.deleteBackwardWord)
      case SelfInsert(s) ⇒
        for (c ← s)
          state.updateLineBuffer(_.addCharacterAtCursor(c))
      case AssistInvocation ⇒
        if (state.assistanceStateOpt.isDefined)
          state.assistanceStateOpt = None
        else
          updateInvocationAssistance()
      case IncrementalHistorySearch ⇒
        state.incrementalSearchStateOpt = Some(IncrementalSearchState("", 0))
      case YankLastArg ⇒
        yankLastArg()
      case ToggleQuote ⇒
        state.updateLineBuffer(buff ⇒ QuoteToggler.toggleQuotes(buff, state.mish))
      case ToggleMish ⇒
        toggleMish()
      case _ ⇒
    }
    if (action != YankLastArg && action != ClearScreen)
      state.yankLastArgStateOpt = None
  }

  private def toggleMish() {
    state.lineBuffer =
      if (state.lineBuffer.text startsWith "!")
        state.lineBuffer.delete(0)
      else
        state.lineBuffer.insertCharacters("!", 0)
  }

  private def yankLastArg() {
    val (argIndex, oldRegion) = state.yankLastArgStateOpt match {
      case Some(YankLastArgState(n, region)) ⇒ (n + 1, region)
      case None                              ⇒ (0, Region(state.lineBuffer.cursorPos, 0))
    }
    state.history.getLastArg(argIndex) match {
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

    state.history.resetHistoryPosition()

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
      System.out.write(renderResult.screen.acceptScreen.getBytes)
      System.out.flush()
    }
  }

  private def runCommand(cmd: String) {
    val commandRunner = new CommandRunner(output, terminal.info, getEnvironment)
    val CommandResult(resultOpt, toggleMish, insertReferenceOpt) =
      try
        commandRunner.run(cmd, state.mish, state.bareWords)
      catch {
        case e: Exception ⇒
          e.printStackTrace()
          DebugLogger.logException(e)
      }
    if (toggleMish)
      state.mish = !state.mish
    else
      state.history.record(cmd, state.mish)

    for (result ← resultOpt) {
      state.globalVariables += ReplState.It -> result
      val newResults =
        state.globalVariables.get(ReplState.Res) match {
          case Some(MashList(oldResults @ _*)) ⇒ MashList(oldResults :+ result)
          case _                               ⇒ MashList(Seq(result))
        }
      state.globalVariables += ReplState.Res -> newResults

      for (resultIndex ← insertReferenceOpt) {
        val resultsIndex = newResults.size - 1
        val toInsert = s"${ReplState.Res}[$resultsIndex][$resultIndex]"
        state.lineBuffer = state.lineBuffer.addCharactersAtCursor(toInsert)
      }

    }
  }

  private def handleComplete() =
    for (result ← complete)
      result.completions match {
        case Seq(completion) ⇒ immediateInsert(completion, result)
        case _               ⇒ enterIncrementalCompletionState(result)
      }

  private def immediateInsert(completion: Completion, result: CompletionResult) {
    val newText = result.replacementLocation.replace(state.lineBuffer.text, completion.replacement)
    val newCursorPos = result.replacementLocation.offset + completion.replacement.length
    state.lineBuffer = LineBuffer(newText, newCursorPos)
  }

}