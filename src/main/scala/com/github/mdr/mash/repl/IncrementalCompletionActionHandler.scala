package com.github.mdr.mash.repl

import com.github.mdr.mash.completions.CompletionResult
import com.github.mdr.mash.input.InputAction

trait IncrementalCompletionActionHandler { self: Repl ⇒
  import InputAction._

  protected def handleIncrementalCompletionAction(action: InputAction, completionState: IncrementalCompletionState) {
    action match {
      case SelfInsert(s) ⇒
        handleInsert(s, completionState)
      case BackwardDeleteChar if completionState.mementoOpt.isDefined ⇒
        completionState.mementoOpt.foreach(_.restoreInto(state))
      case Complete if completionState.immediatelyAfterCompletion ⇒ // enter browse completions mode
        browseCompletion(completionState, activeCompletion = 0)
      case _ ⇒
        state.completionStateOpt = None
        handleNormalAction(action)
    }
  }

  private def handleInsert(s: String, completionState: IncrementalCompletionState) {
    val memento = ReplStateMemento(state.lineBuffer, completionState)
    for (c ← s)
      state.updateLineBuffer(_.addCharacterAtCursor(c))
    state.completionStateOpt = None
    for (CompletionResult(completions, nextReplacementLocation) ← complete) {
      val stillReplacingSameLocation = nextReplacementLocation.offset == completionState.replacementLocation.offset
      if (stillReplacingSameLocation) {
        val replacedText = nextReplacementLocation.of(state.lineBuffer.text)
        completions match {
          case Seq(completion) if replacedText == completion.replacement ⇒
          // ... we leave incremental mode if what the user has typed is an exact much for the sole completion
          case _ ⇒
            val newCompletionState = IncrementalCompletionState(Some(memento), completions, nextReplacementLocation,
              immediatelyAfterCompletion = false)
            state.completionStateOpt = Some(newCompletionState)
        }
      }
    }
  }

}
