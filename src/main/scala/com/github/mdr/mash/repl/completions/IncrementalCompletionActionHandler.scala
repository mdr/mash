package com.github.mdr.mash.repl.completions

import com.github.mdr.mash.completions.CompletionResult
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.{ LineBuffer, Repl }
import com.github.mdr.mash.utils.Region

trait IncrementalCompletionActionHandler { self: Repl ⇒

  protected def enterIncrementalCompletionState(result: CompletionResult) {
    val CompletionResult(completions, replacementLocation @ Region(offset, _)) = result
    val common = result.getCommonInsertText
    val newReplacementLocation = Region(offset, common.length)
    val completionState = IncrementalCompletionState(completions, newReplacementLocation,
      immediatelyAfterCompletion = true)
    state.completionStateOpt = Some(completionState)
    val newText = replacementLocation.replace(state.lineBuffer.text, common)
    val newCursorPos = newReplacementLocation.posAfter - (if (result.allQuoted) 1 else 0)
    state.lineBuffer = LineBuffer(newText, newCursorPos)
    state.assistanceStateOpt = None
  }

  protected def handleIncrementalCompletionAction(action: InputAction, completionState: IncrementalCompletionState) =
    action match {
      case SelfInsert(s) ⇒
        handleInsert(s, completionState)
      case BackwardDeleteChar if completionState.mementoOpt.isDefined ⇒ // restore previous state
        completionState.mementoOpt.foreach(_.restoreInto(state))
      case Complete if completionState.immediatelyAfterCompletion ⇒ // enter browse completions mode
        browseCompletions(completionState)
      case _ ⇒ // exit back to normal mode, and handle there
        state.completionStateOpt = None
        handleNormalAction(action)
    }

  private def handleInsert(s: String, completionState: IncrementalCompletionState) {
    val memento = ReplStateMemento(state.lineBuffer, completionState.copy(immediatelyAfterCompletion = false))
    for (c ← s)
      state.updateLineBuffer(_.addCharacterAtCursor(c))

    state.completionStateOpt = None
    for (CompletionResult(completions, location) ← complete) {
      val previousLocation = completionState.replacementLocation
      val stillReplacingSameLocation = location.offset == previousLocation.offset
      if (stillReplacingSameLocation) {
        val replacedText = location.of(state.lineBuffer.text)
        completions match {
          case Seq(completion) if replacedText == completion.replacement ⇒
          // ... we leave incremental mode if what the user has typed is an exact much for the sole completion
          case _ ⇒
            val newCompletionState = IncrementalCompletionState(completions, location,
              immediatelyAfterCompletion = false, Some(memento))
            state.completionStateOpt = Some(newCompletionState)
        }
      }
    }
  }

}