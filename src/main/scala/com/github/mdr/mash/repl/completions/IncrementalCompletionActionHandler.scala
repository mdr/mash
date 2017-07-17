package com.github.mdr.mash.repl.completions

import com.github.mdr.mash.completions.CompletionResult
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.{ LineBuffer, Repl }
import com.github.mdr.mash.utils.Region

trait IncrementalCompletionActionHandler {
  self: Repl ⇒

  protected def enterIncrementalCompletionState(result: CompletionResult) {
    val (completionState, newLineBuffer) = initialIncrementalCompletionState(result, state.lineBuffer)
    state = state.copy(
      completionStateOpt = Some(completionState),
      lineBuffer = newLineBuffer,
      assistanceStateOpt = None)
  }

  protected def initialIncrementalCompletionState(result: CompletionResult, lineBuffer: LineBuffer): (IncrementalCompletionState, LineBuffer) = {
    val CompletionResult(completions, replacementLocation@Region(offset, _)) = result
    val common = result.getCommonInsertText
    val newReplacementLocation = Region(offset, common.length)
    val completionState = IncrementalCompletionState(completions, newReplacementLocation,
      immediatelyAfterCompletion = true)
    val newText = replacementLocation.replace(lineBuffer.text, common)
    val newCursorPos = newReplacementLocation.posAfter - (if (result.allQuoted) 1 else 0)
    val newLineBuffer = LineBuffer(newText, newCursorPos)
    (completionState, newLineBuffer)
  }

  protected def handleIncrementalCompletionAction(action: InputAction, completionState: IncrementalCompletionState) =
    action match {
      case SelfInsert(s)                                              ⇒
        handleInsert(s, completionState)
      case BackwardDeleteChar if completionState.mementoOpt.isDefined ⇒ // restore previous state
        completionState.mementoOpt.foreach(memento ⇒ state = memento.restoreInto(state))
      case Complete if completionState.immediatelyAfterCompletion     ⇒ // enter browse completions mode
        browseCompletions(completionState)
      case _                                                          ⇒ // exit back to normal mode, and handle there
        state = state.exitCompletion
        handleNormalAction(action)
    }

  private def handleInsert(characters: String, completionState: IncrementalCompletionState) {
    val (newLineBuffer, newCompletionStateOpt) = getNewIncrementalSearchState(characters, state.lineBuffer, completionState, state.mish)
    state = state.copy(
      completionStateOpt = newCompletionStateOpt,
      lineBuffer = newLineBuffer)
  }

  protected def getNewIncrementalSearchState(insertedCharacters: String,
                                             lineBuffer: LineBuffer,
                                             completionState: IncrementalCompletionState,
                                             mish: Boolean): (LineBuffer, Option[IncrementalCompletionState]) = {
    val memento = ReplStateMemento(lineBuffer, completionState.copy(immediatelyAfterCompletion = false))
    val newLineBuffer = lineBuffer.addCharactersAtCursor(insertedCharacters)
    var newCompletionStateOpt: Option[IncrementalCompletionState] = None
    for (CompletionResult(completions, location) ← complete(newLineBuffer, mish)) {
      val previousLocation = completionState.replacementLocation
      val stillReplacingSameLocation = location.offset == previousLocation.offset
      if (stillReplacingSameLocation) {
        val replacedText = location.of(newLineBuffer.text)
        completions match {
          case Seq(completion) if replacedText == completion.replacement ⇒
          // ... we leave incremental mode if what the user has typed is an exact much for the sole completion
          case _ ⇒
            val newCompletionState = IncrementalCompletionState(completions, location,
              immediatelyAfterCompletion = false, Some(memento))
            newCompletionStateOpt = Some(newCompletionState)
        }
      }
    }
    (newLineBuffer, newCompletionStateOpt)
  }

}