package com.github.mdr.mash.repl

object UndoRedoState {

  val Clean = UndoRedoState()

}

case class UndoRedoState(previousLineBuffers: List[LineBuffer] = List()) {

  def push(lineBuffer: LineBuffer): UndoRedoState = copy(previousLineBuffers = lineBuffer :: previousLineBuffers)

  def pop: Option[(LineBuffer, UndoRedoState)] = previousLineBuffers match {
    case Nil          ⇒ None
    case head :: tail ⇒ Some(head, copy(previousLineBuffers = tail))
  }

}
