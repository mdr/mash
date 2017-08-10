package com.github.mdr.mash.repl

import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.repl.NormalActions._

import scala.PartialFunction.condOpt

object LineBufferActionHandler {

  private def noCopy(f: LineBuffer ⇒ LineBuffer): LineBuffer ⇒ LineBufferResult =
    lineBuffer ⇒ LineBufferResult(f(lineBuffer))

  def unapply(action: InputAction): Option[LineBuffer ⇒ LineBufferResult] = condOpt(action) {
    case SelfInsert(c)                  ⇒ noCopy(_.insertAtCursor(c))
    case BeginningOfLine                ⇒ noCopy(_.moveCursorToStart)
    case EndOfLine                      ⇒ noCopy(_.moveCursorToEnd)
    case ForwardChar                    ⇒ noCopy(_.cursorRight())
    case BackwardChar                   ⇒ noCopy(_.cursorLeft())
    case ForwardCharExtendingSelection  ⇒ noCopy(_.cursorRight(extendSelection = true))
    case BackwardCharExtendingSelection ⇒ noCopy(_.cursorLeft(extendSelection = true))
    case ForwardWordExtendingSelection  ⇒ noCopy(_.forwardWord(extendSelection = true))
    case BackwardWordExtendingSelection ⇒ noCopy(_.backwardWord(extendSelection = true))
    case ForwardWord                    ⇒ noCopy(_.forwardWord())
    case BackwardWord                   ⇒ noCopy(_.backwardWord())
    case DeleteChar                     ⇒ noCopy(_.delete)
    case BackwardDeleteChar             ⇒ noCopy(_.backspace)
    case KillLine                       ⇒ noCopy(_.deleteToEndOfLine)
    case BackwardKillLine               ⇒ noCopy(_.deleteToBeginningOfLine)
    case KillWord                       ⇒ _.deleteForwardWord
    case BackwardKillWord               ⇒ _.deleteBackwardWord
  }

}
