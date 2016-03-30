package com.github.mdr.mash

import com.github.mdr.mash.completions.CompletionResult
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.incrementalSearch.IncrementalSearchState
import com.github.mdr.mash.completions.UberCompleter
import com.github.mdr.mash.parser.StringEscapes
import com.github.mdr.mash.evaluator.MashList

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
        yankLastArg()
      case QuoteWord ⇒
        quoteWord()
      case _ ⇒
    }
    if (action != YankLastArg && action != ClearScreen)
      state.yankLastArgStateOpt = None
  }

  private def quoteWord() {
    val oldS = state.lineBuffer.s
    val cursorPos = state.lineBuffer.cursorPos
    val cursorRegion = if (cursorPos > 0) Region(cursorPos - 1, 1) else Region(cursorPos, 0)
    val region = UberCompleter.getContiguousRegion(oldS, cursorRegion, state.mish)
    val toQuote = region.of(oldS)
    val quoted = "\"" + StringEscapes.escapeChars(toQuote) + "\""
    val newS = region.replace(state.lineBuffer.s, quoted)
    state.lineBuffer = LineBuffer(newS, region.offset + quoted.length)
  }

  private def yankLastArg() {
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
  }

  private def handleAcceptLine() {
    updateScreenAfterAccept()
    previousReplRenderResultOpt = None

    state.history.resetHistoryPosition()

    val cmd = state.lineBuffer.s
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

  private def handleComplete() {
    val completionResult = complete
    for (CompletionResult(completions, replacementLocation) ← completionResult) {
      val Region(offset, length) = replacementLocation
      completions match {
        case Seq() ⇒ // no completions: do nothing
        case Seq(completion) ⇒ // a unique completion: immediate insert
          val newS = replacementLocation.replace(state.lineBuffer.s, completion.replacement)
          val newCursorPos = offset + completion.replacement.length
          state.lineBuffer = LineBuffer(newS, newCursorPos)
        case _ ⇒ // multiple completions
          var completionState = IncrementalCompletionState(None, completions, replacementLocation,
            immediatelyAfterCompletion = true)
          val newReplacementRegion = Region(offset, completionState.getReplacement.length)
          completionState = completionState.copy(replacementLocation = newReplacementRegion)
          state.completionStateOpt = Some(completionState)
          val newS = replacementLocation.replace(state.lineBuffer.s, completionState.getReplacement)
          val newCursorPos = if (completionState.allQuoted) newReplacementRegion.posAfter - 1 else newReplacementRegion.posAfter
          state.lineBuffer = LineBuffer(newS, newCursorPos)
          state.assistanceStateOpt = None
      }
    }
  }

}