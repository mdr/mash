package com.github.mdr.mash.repl

import com.github.mdr.mash.completions.CompletionResult
import com.github.mdr.mash.input.InputAction

trait IncrementalCompletionActionHandler { self: Repl ⇒
  import InputAction._

  protected def handleIncrementalCompletionAction(action: InputAction, completionState: IncrementalCompletionState) {
    action match {
      case SelfInsert(s) ⇒
        val priorState = PriorIncrementalCompleteState(state.lineBuffer, completionState)
        for (c ← s)
          state.updateLineBuffer(_.addCharacterAtCursor(c))
        refreshCompletions(priorState)
      case BackwardDeleteChar if completionState.priorCompletionStateOpt.isDefined ⇒
        for (priorCompletionState ← completionState.priorCompletionStateOpt)
          priorCompletionState.unwind(state)
      case Complete if completionState.immediatelyAfterCompletion ⇒ // enter browse completions mode
        state.assistanceStateOpt = None
        browseCompletion(completionState, 0)
      case _ ⇒
        state.completionStateOpt = None
        handleNormalAction(action)
    }
  }

  private def refreshCompletions(priorState: PriorIncrementalCompleteState) {
    state.completionStateOpt = None
    for (CompletionResult(completions, nextReplacementLocation) ← complete) {
      val stillReplacingSameLocation = nextReplacementLocation.offset == priorState.completionState.replacementLocation.offset
      if (stillReplacingSameLocation) {
        val replacedText = nextReplacementLocation.of(state.lineBuffer.text)
        completions match {
          case Seq(completion) if replacedText == completion.replacement ⇒
          // ... we leave incremental mode if what the user has typed is an exact much for the sole completion
          case _ ⇒
            val newCompletionState = IncrementalCompletionState(Some(priorState), completions, nextReplacementLocation,
              immediatelyAfterCompletion = false)
            state.completionStateOpt = Some(newCompletionState)
        }
      }
    }
  }

}
