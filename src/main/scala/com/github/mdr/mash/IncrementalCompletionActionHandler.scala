package com.github.mdr.mash

import com.github.mdr.mash.completions.CompletionResult
import com.github.mdr.mash.input.InputAction

trait IncrementalCompletionActionHandler { self: Repl ⇒
  import InputAction._

  protected def handleIncrementalCompletionAction(action: InputAction, completionState: IncrementalCompletionState) {
    action match {
      case SelfInsert(s) ⇒
        for (c ← s)
          state.lineBuffer = state.lineBuffer.addCharacterAtCursor(c)
        refreshCompletions(completionState)
      case BackwardDeleteChar if completionState.priorCompletionStateOpt.isDefined ⇒
        for (PriorIncrementalCompleteState(lineBuffer, completionState) ← completionState.priorCompletionStateOpt) {
          state.lineBuffer = lineBuffer
          state.completionStateOpt = Some(completionState)
        }
      case Complete if completionState.immediatelyAfterCompletion ⇒ // enter browse completions mode
        state.assistanceStateOpt = None
        browseCompletion(completionState, 0)
      case _ ⇒
        state.completionStateOpt = None
        handleNormalAction(action)
    }
  }

  private def refreshCompletions(completionState: IncrementalCompletionState) {
    complete match {
      case Some(CompletionResult(completions, replacementLocation)) ⇒
        val stillReplacingSameLocation = replacementLocation.offset == completionState.replacementLocation.offset
        if (stillReplacingSameLocation) {
          val replacedText = replacementLocation.of(state.lineBuffer.text)
          completions match {
            case Seq() ⇒
              state.completionStateOpt = None
            case Seq(completion) if replacedText == completion.replacement ⇒ // what you've typed is an exact much for the sole completion
              state.completionStateOpt = None
            case _ ⇒
              val priorState = PriorIncrementalCompleteState(state.lineBuffer, completionState)
              val newCompletionState = IncrementalCompletionState(Some(priorState), completions, replacementLocation,
                immediatelyAfterCompletion = false)
              state.completionStateOpt = Some(newCompletionState)
          }
        } else
          state.completionStateOpt = None
      case None ⇒
        state.completionStateOpt = None
    }
  }

}
