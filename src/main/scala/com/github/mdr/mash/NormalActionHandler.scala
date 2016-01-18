package com.github.mdr.mash

import com.github.mdr.mash.completions.CompletionResult
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.incrementalSearch.IncrementalSearchState
import com.github.mdr.mash.completions.UberCompleter
import com.github.mdr.mash.parser.StringEscapes

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
      case SelfInsert(c)      ⇒ state.lineBuffer = state.lineBuffer.addCharacterAtCursor(c)
      case BeginningOfLine    ⇒ state.lineBuffer = state.lineBuffer.moveCursorToStart
      case EndOfLine          ⇒ state.lineBuffer = state.lineBuffer.moveCursorToEnd
      case ForwardChar        ⇒ state.lineBuffer = state.lineBuffer.cursorRight
      case BackwardChar       ⇒ state.lineBuffer = state.lineBuffer.cursorLeft
      case ForwardWord        ⇒ state.lineBuffer = state.lineBuffer.forwardWord
      case BackwardWord       ⇒ state.lineBuffer = state.lineBuffer.backwardWord
      case DeleteChar         ⇒ state.lineBuffer = state.lineBuffer.delete
      case BackwardDeleteChar ⇒ state.lineBuffer = state.lineBuffer.backspace
      case KillLine           ⇒ state.lineBuffer = state.lineBuffer.deleteToEndOfLine
      case KillWord           ⇒ state.lineBuffer = state.lineBuffer.deleteForwardWord
      case BackwardKillWord   ⇒ state.lineBuffer = state.lineBuffer.deleteBackwardWord
      case AssistInvocation ⇒
        if (state.assistanceStateOpt.isDefined)
          state.assistanceStateOpt = None
        else
          updateInvocationAssistance()
      case IncrementalHistorySearch ⇒
        state.incrementalSearchStateOpt = Some(IncrementalSearchState("", 0))
      case YankLastArg ⇒
        val (argIndex, oldRegion) = state.yankLastArgStateOpt match {
          case Some(YankLastArgState(n, region)) ⇒ (n + 1, region)
          case None                              ⇒ (0, Region(state.lineBuffer.cursorPos, 0))
        }
        state.history.getLastArg(argIndex) match {
          case Some(newArg) ⇒
            val newS = oldRegion.replace(state.lineBuffer.s, newArg)
            val newRegion = Region(oldRegion.offset, newArg.length)
            state.lineBuffer = LineBuffer(newS, newRegion.posAfter)
            state.yankLastArgStateOpt = Some(YankLastArgState(argIndex, newRegion))
          case None ⇒
        }
      case QuoteWord ⇒
        val oldS = state.lineBuffer.s
        val cursorPos = state.lineBuffer.cursorPos
        val cursorRegion = if (cursorPos > 0) Region(cursorPos - 1, 1) else Region(cursorPos, 0)
        val region = UberCompleter.getContiguousRegion(oldS, cursorRegion, state.mish)
        val toQuote = region.of(oldS)
        val quoted = "\"" + StringEscapes.escapeChars(toQuote) + "\""
        val newS = region.replace(state.lineBuffer.s, quoted)
        state.lineBuffer = LineBuffer(newS, region.offset + quoted.length)
      case _ ⇒
    }
    if (action != YankLastArg && action != ClearScreen)
      state.yankLastArgStateOpt = None
  }

  private def handleAcceptLine() {
    // Clear completions and assistance pop-ups, just leaving the input line
    state.completionStateOpt = None
    state.assistanceStateOpt = None
    draw()

    // Move past the input line
    for (renderResult ← previousReplRenderResultOpt) {
      System.out.write(renderResult.screen.acceptScreen.getBytes)
      System.out.flush()
    }
    previousReplRenderResultOpt = None

    state.history.resetHistoryPosition()

    // Run command
    val cmd = state.lineBuffer.s
    if (cmd.trim.nonEmpty) {
      try {
        val commandRunner = new CommandRunner(terminalInfo, getEnvironment)
        val CommandResult(resultOpt, toggleMish) = commandRunner.run(cmd, state.mish, state.bareWords)
        if (toggleMish)
          state.mish = !state.mish
        for (result ← resultOpt)
          state.globalVariables += "it" -> result
      } catch {
        case e: Exception ⇒
          e.printStackTrace()
          DebugLogger.logException(e)
      }
      state.history.record(cmd)
    }

    state.lineBuffer = LineBuffer.Empty
  }

  private def handleComplete() {
    val completionResult = complete
    for (CompletionResult(completions, prefix, replacementLocation) ← completionResult) {
      val Region(offset, length) = replacementLocation
      completions match {
        case Seq() ⇒
        case Seq(completion) ⇒ // immediate insert
          val s = state.lineBuffer.s
          var newCursorPos = offset + completion.replacement.length
          state.lineBuffer = LineBuffer(s.take(offset) + completion.replacement + s.drop(offset + length), newCursorPos)
        case _ ⇒ // multiple completions
          val accepted = completions.map(_.text).reduce(StringUtils.commonPrefix)
          var completionState = IncrementalCompletionState(prefix, completions, accepted, replacementLocation, immediatelyAfterCompletion = true)
          val newReplacementRegion = Region(offset, completionState.getReplacement.length)
          completionState = completionState.copy(replacementLocation = newReplacementRegion)
          state.completionStateOpt = Some(completionState)
          val s = state.lineBuffer.s
          val newS = s.take(offset) + completionState.getReplacement + s.drop(offset + length)
          val posAfterCompletion = newReplacementRegion.offset + newReplacementRegion.length
          val newCursorPos = if (completionState.allQuoted) posAfterCompletion - 1 else posAfterCompletion
          state.lineBuffer = LineBuffer(newS, newCursorPos)
          state.assistanceStateOpt = None
      }
    }
  }

}