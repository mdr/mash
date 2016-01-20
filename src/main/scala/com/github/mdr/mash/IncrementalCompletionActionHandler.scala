package com.github.mdr.mash

import com.github.mdr.mash.completions.CompletionResult

trait IncrementalCompletionActionHandler { self: Repl ⇒
  import InputAction._

  protected def handleIncrementalCompletionAction(action: InputAction, completionState: IncrementalCompletionState) {
    action match {
      case SelfInsert(c) ⇒
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
        if (replacementLocation == completionState.replacementLocation) {
          val replacedText = replacementLocation.of(state.lineBuffer.s)
          completions match {
            case Seq() ⇒
              state.completionStateOpt = None
            case Seq(completion) if replacedText == completion.text ⇒
              state.completionStateOpt = None
            case _ ⇒
              val priorState = PriorIncrementalCompleteState(state.lineBuffer, completionState)
              val newCompletionState = IncrementalCompletionState(Some(priorState), completions, replacedText,
                replacementLocation, immediatelyAfterCompletion = false)
              state.completionStateOpt = Some(newCompletionState)
          }
        } else
          state.completionStateOpt = None
      case None ⇒
        state.completionStateOpt = None
    }
  }

}
